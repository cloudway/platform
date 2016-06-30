{{define "pagetitle"}}应用控制台 - 创建应用{{end}}

<div class="row">
  <div class="col-md-6">
    <h3>创建应用<h3>
  </div>
<div>

<div class="row container">
  <div class="panel panel-info col-md-offset-1 col-md-6">
    <div class="panel-body">
      {{if .error}}
      <div class="alert alert-danger">{{.error}}</div>
      {{end}}
      <form action="/applications" method="post">
        <div class="form-group">
          <label for="name">应用名称：</label>
          <div class="input-group col-md-8">
            <span class="input-group-addon">http://</span>
            <input type="text" name="name" class="form-control" value="{{.name}}">
            <span class="input-group-addon">-{{.user.Namespace}}.{{.domain}}</span>
          </div>
        </div>
        <div class="form-group">
          <label for="plugins">应用框架及服务：</label>
          <div class="input-group col-md-12">
            <input type="text" id="plugins" name="plugins" class="form-control" value="{{.plugins}}">
            {{- with .available_plugins}}
            <span class="input-group-btn">
              <button type="button" class="btn btn-default dropdown-toggle" data-toggle="dropdown" aira-hasgroups="true" aria-expanded="false"><span class="caret"></span></button>
              <ul class="dropdown-menu">
                <li class="dropdown-header">框架</li>
                {{- range .}}
                {{- if eq .Category "Framework"}}
                <li><a name="{{.Name}}:{{.Version}}" class="plugin-select" href="#">{{.DisplayName}}</a></li>
                {{- end}}
                {{- end}}
                <li class="dropdown-header">服务</li>
                {{- range .}}
                {{- if eq .Category "Service"}}
                <li><a name="{{.Name}}:{{.Version}}" class="plugin-select" href="#">{{.DisplayName}}</a></li>
                {{- end}}
                {{- end}}
              </ul>
            </span>
            {{- end}}
          </div>
        </div>
        <div class="form-group">
          <label for="repo">代码库：</label>
          <div class="input-group col-md-12">
            <input type="text" name="repo" class="form-control" placeholder="git://github.com/cloudway/" value="{{.repo}}">
          </div>
        </div>
        <input type="hidden" name="csrf_token" value="{{.csrf_token}}"/>
        <button class="btn btn-success" type="submit">创建</button>
        <a class="btn btn-link" href="/applications">取消</a>
      </form>
    </div>
  </div>
</div>

{{template "_select_plugin"}}
