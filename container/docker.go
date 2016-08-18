package container

import "github.com/docker/engine-api/client"

type DockerClient struct {
	*client.Client
}

func NewEnvClient() (DockerClient, error) {
	cli, err := client.NewEnvClient()
	return NewClient(cli), err
}

func NewClient(cli *client.Client) DockerClient {
	return DockerClient{cli}
}
