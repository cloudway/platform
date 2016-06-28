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
          <label for="plugins">应用框架及插件：</label>
          <div class="input-group col-md-12">
            <input type="text" name="plugins" class="form-control" value="{{.plugins}}">
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
