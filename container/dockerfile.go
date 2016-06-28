package container

import "text/template"

var dockerfileTemplate = template.Must(template.New("Dockerfile").Parse(`
FROM {{.BaseImage}}

{{ if .InstallScript -}}
RUN echo '{{.InstallScript}}' | /bin/sh
{{- end }}

{{ if eq .User "root" -}}
RUN mkdir -p {{.Home}}/.env {{.Home}}/repo {{.Home}}/deploy {{.Home}}/data {{.Home}}/logs
{{- else -}}
RUN groupadd {{.User}} && useradd -g {{.User}} -d {{.Home}} -m -s /usr/bin/cwsh {{.User}} \
 && mkdir -p {{.Home}}/.env {{.Home}}/repo {{.Home}}/deploy {{.Home}}/data {{.Home}}/logs \
 && chown -R {{.User}}:{{.User}} {{.Home}} \
 && chown root:root {{.Home}}/.env
{{- end }}

WORKDIR {{.Home}}

COPY support /
ADD {{.PluginInstallPath}} /tmp/install_{{.Plugin.Name}}

ENV{{range $key, $value := .Env}}{{printf " %s=%q" $key $value}}{{end}}

RUN /usr/bin/cwctl {{if .Debug}}--debug{{end}} install {{.Plugin.Name}} /tmp/install_{{.Plugin.Name}}
`))
