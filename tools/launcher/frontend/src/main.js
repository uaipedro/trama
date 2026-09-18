import './style.css';
import './app.css';

import { CheckEnvironment, EnsureRPortable, InstallCollections, OpenTrama } from '../wailsjs/go/main/App';

function showScreen(id) {
    document.querySelectorAll('[data-screen]').forEach((el) => {
        el.hidden = el.id !== id;
    });
}

const installLog = document.getElementById('install-log');
const btnInstall = document.getElementById('btn-install');
const btnOpen = document.getElementById('btn-open');

async function init() {
    showScreen('screen-checking');
    try {
        const status = await CheckEnvironment();
        if (!status.RPortableInstalled) {
            await EnsureRPortable();
        }
        if (status.TramaInstalled) {
            showScreen('screen-ready');
            return;
        }
        showScreen('screen-collections');
    } catch (err) {
        console.error(err);
        installLog.textContent = `Erro ao checar o ambiente: ${err}`;
        showScreen('screen-collections');
    }
}

btnInstall.addEventListener('click', async () => {
    const selected = Array.from(
        document.querySelectorAll('#screen-collections input:checked')
    ).map((el) => el.value);

    btnInstall.disabled = true;
    installLog.textContent = 'Instalando…';
    try {
        await InstallCollections(selected);
        showScreen('screen-ready');
    } catch (err) {
        console.error(err);
        installLog.textContent = `Erro ao instalar: ${err}`;
    } finally {
        btnInstall.disabled = false;
    }
});

btnOpen.addEventListener('click', async () => {
    btnOpen.disabled = true;
    try {
        await OpenTrama();
    } catch (err) {
        console.error(err);
        alert(`Erro ao abrir o trama: ${err}`);
    } finally {
        btnOpen.disabled = false;
    }
});

init();
