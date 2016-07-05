{{define "pagetitle"}}Error{{end}}

<div class="row">
  <div class="col-md-offset-1 col-md 10">
    <h4>{{.error_message}}</h4>
    <a class="btn btn-xs" href="{{.return_path}}">返回</a>
  </div>
</div>
