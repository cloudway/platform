package api

import (
	"strconv"
	"strings"
)

func CompareVersions(v1, v2 string) int {
	switch {
	case v1 == v2:
		return 0
	case v1 == "":
		return -1
	case v2 == "":
		return 1
	}

	currTab := strings.Split(v1, ".")
	otherTab := strings.Split(v2, ".")

	max := len(currTab)
	if len(otherTab) > max {
		max = len(otherTab)
	}

	for i := 0; i < max; i++ {
		var currInt, otherInt int
		if len(currTab) > i {
			currInt, _ = strconv.Atoi(currTab[i])
		}
		if len(otherTab) > i {
			otherInt, _ = strconv.Atoi(otherTab[i])
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
