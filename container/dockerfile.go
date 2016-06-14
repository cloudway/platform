package container

import "text/template"

var dockerfileTemplate = template.Must(template.New("Dockerfile").Parse(`
FROM {{.BaseImage}}

{{ if .InstallScript -}}
RUN echo '{{.InstallScript}}' | /bin/sh
{{- end }}

{{ if eq .User "root" -}}
RUN mkdir -p {{.Home}}/.env {{.Home}}/repo {{.Home}}/data {{.Home}}/logs
{{- else -}}
RUN groupadd {{.User}} && useradd -g {{.User}} -d {{.Home}} -m -s /usr/bin/cwsh {{.User}} \
 && mkdir -p {{.Home}}/.env {{.Home}}/repo {{.Home}}/data {{.Home}}/logs \
 && chown -R {{.User}}:{{.User}} {{.Home}} \
 && chown root:root {{.Home}}/.env
{{- end }}

WORKDIR {{.Home}}

COPY support /
ADD {{.PluginInstallPath}} /tmp/install_{{.PluginName}}

ENV{{range $key, $value := .Env}}{{printf " %s=%q" $key $value}}{{end}}

{{ if .Debug -}}
RUN /usr/bin/cwctl --debug install {{.PluginName}} /tmp/install_{{.PluginName}}
{{- else -}}
RUN /usr/bin/cwctl install {{.PluginName}} /tmp/install_{{.PluginName}}
{{- end }}
`))
