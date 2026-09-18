package pakrunner

import "testing"

func TestBuildInstallScript(t *testing.T) {
	script := BuildInstallScript([]string{"uaipedro/trama", "uaipedro/trama/collections/trama.data"}, "/home/user/.trama-launcher/lib")

	want := []string{
		`options(repos = c(P3M = "https://packagemanager.posit.co/cran/latest"))`,
		`if (!requireNamespace("pak", quietly = TRUE)) {`,
		`install.packages("pak")`,
		`lib <- "/home/user/.trama-launcher/lib"`,
		`pak::pak(c("uaipedro/trama", "uaipedro/trama/collections/trama.data"), lib = lib)`,
	}
	for _, w := range want {
		if !contains(script, w) {
			t.Fatalf("script gerado não contém %q:\n%s", w, script)
		}
	}
}

func contains(haystack, needle string) bool {
	return len(haystack) >= len(needle) && (func() bool {
		for i := 0; i+len(needle) <= len(haystack); i++ {
			if haystack[i:i+len(needle)] == needle {
				return true
			}
		}
		return false
	})()
}
