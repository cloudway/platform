{{define "pagetitle"}}应用控制台 - 应用{{end}}

<div class="row container">
{{if .apps}}
  {{range .apps}}
  <div class="row container">
    <div class="col-md-2 col-lg-offset-1">
      <a href="/applications/{{.Name}}" style="font-size:160%; color:#555;">{{.Name}}</a>
      <sup> <a href="{{.URL}}" target="_blank"><i class="fa fa-external-link"></i></a></sup>
    </div>
    <div class="col-md-10 col-lg-8 conditional-text-align">
      <div>
        <span class="label label-success">{{.Framework}}</span>
        {{range .Plugins}}
        <span class="label label-info">{{.}}</span>
        {{end}}
      </div>
      <div class="text-muted" style="margin-top:5px;">
        <small>创建于: {{humanDuration .CreatedAt}}</small>
      </div>
    </div>
  </div>
  <hr style="margin-top:10px; margin-bottom:15px;"/>
  {{end}}
  <div class="col-md-2 col-lg-offset-1">
    <a class="btn btn-primary" href="/applications/create/form"><i class="fa fa-plus"></i> 创建应用</a>
  </div>
{{else}}
  <div class="col-md-2 col-lg-offset-1">
    <a class="btn btn-primary" href="/applications/create/form"><i class="fa fa-bolt"></i> 创建你的第一个应用</a>
  </div>
{{end}}
</div>
