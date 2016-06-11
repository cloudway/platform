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

ENV CLOUDWAY_APP_NAME={{printf "%q" .Name}} \
    CLOUDWAY_APP_NAMESPACE={{printf "%q" .Namespace}} \
    CLOUDWAY_APP_DNS={{printf  "%q" .FQDN}} \
    CLOUDWAY_APP_USER={{printf "%q" .User}} \
    CLOUDWAY_HOME_DIR={{printf "%q" .Home}} \
    CLOUDWAY_LOG_DIR={{printf  "%q" (printf "%s/logs" .Home)}} \
    CLOUDWAY_DATA_DIR={{printf "%q" (printf "%s/data" .Home)}} \
    CLOUDWAY_REPO_DIR={{printf "%q" (printf "%s/repo" .Home)}}

{{ if .Debug -}}
RUN /usr/bin/cwctl install --debug {{.PluginName}} /tmp/install_{{.PluginName}}
{{- else -}}
RUN /usr/bin/cwctl install {{.PluginName}} /tmp/install_{{.PluginName}}
{{- end }}
`))
