<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE-edge">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>{{template "pagetitle" .}}</title>
  <link rel="stylesheet" href="//cdn.bootcss.com/bootstrap/3.3.2/css/bootstrap.min.css" />
  <link rel="stylesheet" href="//cdn.bootcss.com/font-awesome/4.3.0/css/font-awesome.min.css" />
  <script type="text/javascript" src="//cdn.bootcss.com/jquery/2.1.3/jquery.min.js"></script>
  <script type="text/javascript" src="//cdn.bootcss.com/bootstrap/3.3.2/js/bootstrap.min.js"></script>
  <link rel="stylesheet" href="/static/css/main.css" />
  {{template "prelude" .}}
</head>
<body class="container" style="padding-top: 15px;">
  <nav class="navbar navbar-default">
    <div class="container-fluid">
      <div class="navbar-header">
        <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
          <span class="icon-bar"></span>
        </button>
        <a class="navbar-brand" href="/">应用控制台</a>
      </div>

      <div id="navbar" class="collapse navbar-collapse">
        <ul class="nav navbar-nav navbar-right">
          {{if not .loggedin}}
          <li><a href="/auth/register">注册</a></li>
          <li><a href="/auth/login"><i class="fa fa-sign-in"></i> 登录</a></li>
          {{else}}
          <li class="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">
              <img src="{{gravatar .user.Name 20}}"/> {{.user.Name}}<span class="caret"></span>
            </a>
            <ul class="dropdown-menu" role="menu">
              <li><a href="#">帮助</a></li>
              <li><a href="/settings">设置</a></li>
              <li><a href="/password">修改密码</a></li>
              <li role="separator" class="divider"></li>
              <li>
                <a href="/auth/logout">
                  <i class="fa fa-sign-out"></i> 注销
                </a>
              </li>
            </ul>
          </li>
          {{end}}
        </ul>
      </div>
    </div>
  </nav>

  {{with .flash_success}}<div class="alert alert-success">{{.}}</div>{{end}}
  {{with .flash_error}}<div class="alert alert-danger">{{.}}</div>{{end}}
  {{template "yield" .}}
  {{template "authboss" .}}
</body>
</html>
{{define "pagetitle"}}{{end}}
{{define "prelude"}}{{end}}
{{define "yield"}}{{end}}
{{define "authboss"}}{{end}}
