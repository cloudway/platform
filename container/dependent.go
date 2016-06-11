package container

import "github.com/cloudway/platform/plugin"

// Resolve service dependencies.
func ResolveServiceDependencies(cs []*Container) error {
    nodes := make([]*node, len(cs))
    for i, c := range cs {
        nodes[i] = &node{data: c}
    }

    // build the dependency graph
    for i := range cs {
        if cs[i].Category() == plugin.Framework {
            // framework plugin depends on all services
            for j := range cs {
                if cs[j].Category() != plugin.Framework {
                    nodes[i].dependsOn(nodes[j])
                }
            }
        } else {
            for _, dep := range cs[i].DependsOn() {
                for j := range cs {
                    // FIXME: enhancement dependent matching
                    if dep == cs[j].ServicePlugin() {
                        nodes[i].dependsOn(nodes[j])
                    }
                }
            }
        }
    }

    // resolve the dependencies by using topological sorting
    nodes, err := TSort(nodes)
    if err == nil {
        for i, n := range nodes {
            cs[i] = n.data.(*Container)
        }
    }
    return err
}
