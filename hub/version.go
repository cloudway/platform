package hub

import (
    "os"
    "sort"
    "strings"
    "strconv"
)

type versions [][]int

func (vs versions) Len() int           { return len(vs) }
func (vs versions) Swap(i, j int)      { vs[i], vs[j] = vs[j], vs[i] }
func (vs versions) Less(i, j int) bool { return compareVersions(vs[i], vs[j]) < 0 }

func getAllVersions(dir string) (versions, error) {
    f, err := os.Open(dir)
    if err != nil {
        return nil, err
    }

    names, err := f.Readdirnames(0)
    f.Close()
    if err != nil {
        return nil, err
    }

    var vers versions
    for _, name := range names {
        if vtab := splitVersion(name); vtab != nil {
            vers = append(vers, vtab)
        }
    }
    sort.Sort(vers)
    return vers, nil
}

func splitVersion(ver string) []int {
    tab := strings.Split(ver, ".")
    ints := make([]int, len(tab))
    for i, x := range tab {
        v, err := strconv.Atoi(x)
        if err != nil {
            return nil
        }
        ints[i] = v
    }
    return ints
}

func joinVersion(ints []int) string {
    tab := make([]string, len(ints))
    for i, x := range ints {
        tab[i] = strconv.Itoa(x)
    }
    return strings.Join(tab, ".")
}

func compareVersions(v1, v2 []int) int {
    max := len(v1)
    if len(v2) > max {
        max = len(v2)
    }

    for i := 0; i < max; i++ {
        var currInt, otherInt int
        if len(v1) > i {
            currInt = v1[i]
        }
        if len(v2) > i {
            otherInt = v2[i]
        }
        if currInt > otherInt {
            return 1
        }
        if otherInt > currInt {
            return -1
        }
    }
    return 0
}

func matchVersion(versions versions, ver string) string {
    if len(ver) == 0 {
        if len(versions) != 0 {
            return joinVersion(versions[len(versions)-1])
        } else {
            return ""
        }
    }

    vtab := splitVersion(ver)
    if vtab == nil {
        return ""
    }

    var max = len(vtab)
    var lastMatch []int
comparing:
    for _, v := range versions {
        if len(v) < max {
            continue
        }
        skip := false
        for i := 0; i < max; i++ {
            if v[i] > vtab[i] {
                break comparing
            }
            if v[i] < vtab[i] {
                skip = true
                break
            }
        }
        if !skip {
            lastMatch = v
        }
    }

    return joinVersion(lastMatch)
}
