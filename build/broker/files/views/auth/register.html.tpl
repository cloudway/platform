{{define "pagetitle"}}用户注册{{end}}

<div class="row">
  <div class="col-md-offset-4 col-md-4">
    <div class="panel panel-default">
      <div class="panel-heading">新用户注册</div>
      <div class="panel-body">
        <form method="POST">
          {{$pid := .primaryID}}
          <div class="form-group {{with .errs}}{{with $errlist := index . $pid}}has-error{{end}}{{end}}">
            <input type="text" class="form-control" name="{{.primaryID}}" placeholder="邮箱" value="{{.primaryIDValue}}" />
            {{with .errs}}{{with $errlist := index . $pid}}{{range $errlist}}<span class="help-block">{{.}}</span>{{end}}{{end}}{{end}}
          </div>
          <div class="form-group {{with .errs}}{{with $errlist := index . "password"}}has-error{{end}}{{end}}">
            <input type="password" class="form-control" name="password" placeholder="密码" value="{{.password}}" />
            {{with .errs}}{{with $errlist := index . "password"}}{{range $errlist}}<span class="help-block">{{.}}</span>{{end}}{{end}}{{end}}
          </div>
          <div class="form-group {{with .errs}}{{with $errlist := index . "confirm_password"}}has-error{{end}}{{end}}">
            <input type="password" class="form-control" name="confirm_password" placeholder="确认密码" value="{{.confirmPassword}}" />
            {{with .errs}}{{with $errlist := index . "confirm_password"}}{{range $errlist}}<span class="help-block">{{.}}</span>{{end}}{{end}}{{end}}
          </div>
          <input type="hidden" name="{{.xsrfName}}" value="{{.xsrfToken}}" />
          <div class="row">
            <div class="col-md-offset-1 col-md-10">
              <button class="btn btn-primary btn-block" type="submit">注册</button>
            </div>
          </div>
          <div class="row">
            <div class="col-md-offset-1 col-md-10">
              <a class="btn btn-link btn-block" href="{{mountpathed "login"}}">取消</a>
            </div>
          </div>
        </form>
      </div>
    </div>
  </div>
</div>