package api

import "testing"

func assertVersion(t *testing.T, v1, v2 string, result int) {
	if r := CompareVersions(v1, v2); r != result {
		t.Fatalf("Unexpected version comparison result. Compare(%s, %s), found %d, expected %d", v1, v2, r, result)
	}
}

func TestCompareVersions(t *testing.T) {
	assertVersion(t, "1.12", "1.12", 0)
	assertVersion(t, "1.0.0", "1", 0)
	assertVersion(t, "1", "1.0.0", 0)
	assertVersion(t, "1", "1.0.1", -1)
	assertVersion(t, "1.0.1", "1", 1)
	assertVersion(t, "1.0.1", "1.0.2", -1)
	assertVersion(t, "1.0.2", "1.0.1", 1)
	assertVersion(t, "1.0.3", "1.1", -1)
	assertVersion(t, "1.1", "1.0.3", 1)
	assertVersion(t, "1.05.00.0156", "1.5.0.156", 0)
	assertVersion(t, "1.5.0.156", "1.05.00.0156", 0)
	assertVersion(t, "", "1.0", -1)
	assertVersion(t, "1.0", "", 1)
	assertVersion(t, "", "", 0)
}
