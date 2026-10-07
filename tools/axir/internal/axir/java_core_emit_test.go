package axir

import "testing"

func TestJavaNamesEscapeKeywordsWithoutCollisions(t *testing.T) {
	for input, want := range map[string]string{
		"%class": "_ax_keyword_class", "%null": "_ax_keyword_null",
		"%boolean": "_ax_keyword_boolean", "%_": "_ax_keyword__",
		"%class_": "class_", "%name": "name",
		"%_ax_keyword_class": "_ax_keyword__ax_keyword_class",
	} {
		if got := javaName(input); got != want {
			t.Errorf("javaName(%q) = %q, want %q", input, got, want)
		}
		if got := javaLiteral(input); got != want {
			t.Errorf("javaLiteral(%q) = %q, want %q", input, got, want)
		}
	}
}
