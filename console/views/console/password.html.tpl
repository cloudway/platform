{{define "pagetitle"}}修改密码{{end}}

<div class="row">
  <div class="col-md-offset-4 col-md-4">
    <div class="panel panel-default">
      <div class="panel-heading">修改密码</div>
      <div class="panel-body">
        {{if .error}}
        <div class="alert alert-danger">{{.error}}</div>
        {{end}}
        <form method="POST">
          <div class="form-group">
            <input type="password" class="form-control" name="oldPassword" placeholder="原密码">
          </div>
          <div class="form-group">
            <input type="password" class="form-control" name="newPassword" placeholder="新密码">
          </div>
          <div class="form-group">
            <input type="password" class="form-control" name="confirmPassword" placeholder="确认密码">
          </div>
          <input type="hidden" name="csrf_token" value="{{.csrf_token}}" />
          <div class="row">
            <div class="col-md-offset-1 col-md-10 text-center">
              <button class="btn btn-primary" type="submit">确认</button>
              <a class="btn btn-link" href="/">取消</a>
            </div>
          </div>
        </form>
      </div>
    </div>
  </div>
</div>
