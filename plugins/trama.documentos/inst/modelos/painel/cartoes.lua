-- Modelo Dashboard do trama: cada região do trama (::: {#trama-… .trama})
-- vira um cartão próprio, em vez de dividir um cartão com o texto em volta.
function Div(el)
  if el.classes:includes("trama") and not el.classes:includes("card") then
    el.classes:insert("card")
    return el
  end
end
