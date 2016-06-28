{{define "pagetitle"}}应用控制台 - 应用{{end}}

<div class="row container">
{{if .apps}}
  {{range .apps}}
  <div class="panel panel-default col-md-12">
    <div class="panel-body">
      <div class="row" style="margin-bottom: 5px;">
        <div class="col-md-2" style="font-size: 200%;">
          <a href="/applications/{{.Name}}">{{.Name}}</a>
        </div>
        <div class="col-md-10 text-right">
          <a href="{{.URL}}" target="_blank"><i class="fa fa-external-link"></i></a>
        </div>
      </div>
      <div class="row">
        <div class="col-md-9">
          <span class="label label-success">{{.Framework}}</span>
          {{range .Plugins}}
          <span class="label label-default">{{.}}</span>
          {{end}}
        </div>
        <div class="col-md-3 text-right">
          创建于：{{formatDate .CreatedAt}}
        </div>
      </div>
    </div>
  </div>
  {{end}}
  <div class="col-md-2">
    <a class="btn btn-primary" href="/forms/applications"><i class="fa fa-plus"></i> 创建应用</a>
  </div>
{{else}}
  <div class="col-md-2">
    <a class="btn btn-primary" href="/forms/applications"><i class="fa fa-bolt"></i> 创建你的第一个应用</a>
  </div>
{{end}}
</div>
