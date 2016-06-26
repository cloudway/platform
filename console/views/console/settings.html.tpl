{{define "pagetitle"}}应用控制台 - 设置{{end}}

<div class="row container">
{{if .current_user.Namespace}}
  <div class="panel panel-info col-md-12" style="margin-bottom: 20px;">
    <h4>名字空间</h4>
    <p>通过名字空间组织你的应用，并赋予每个应用一个唯一的域名</p>
    <div class="col-md col-md-10" style="margin-bottom: 10px;">
      <div>当前正在使用的名字空间：{{.current_user.Namespace}}
      {{if eq (len .current_user.Applications) 0}}
        <a class="fa fa-remove" href="/settings/namespace/delete"></a>
      {{end}}
      </div>
    </div>
  </div>

  <div class="panel panel-info col-md-12">
    <h4>SSH公共密钥</h4>
    <p>通过SSH公共密钥安全上传代码</p>

    <div class="col-md-12" style="margin-top: 10px;">
      {{if ne (len .sshkeys) 0}}
      <div class="panel panel-default">
        <table class="table">
          <tr><th>标签</th><th>内容</th><th></th></tr>
          {{range .sshkeys}}
          <tr>
            <td>{{.Label}}</td>
            <td>{{printf "%.64s..." .Text}}</td>
            <td><a class="fa fa-remove" href="/settings/sshkey/delete?key={{.Text}}"></a></td>
          </tr>
          {{end}}
        </table>
      </div>
      {{end}}

      <div class="row" style="margin-bottom: 20px;">
        <div class="col-md-2">
          <a class="btn btn-primary" href="/settings/sshkey"><i class="fa fa-plus"></i> 增加新密钥...</a>
        </div>
      </div>
    </div>
  </div>

{{else}}

  <div class="panel panel-info col-md-12">
    <h4>名字空间</h4>
    <form action="/settings/namespace" method="post">
      <p>通过名字空间组织你的应用，并赋予每个应用一个唯一的域名</p>
      <div class="form-group">
        <input name="namespace" type="text" size="20" />
        <input type="hidden" name="csrf_token" value="{{.csrf_token}}" />
        <button class="btn btn-success" type="submit">创建名字空间...</button>
      </div>
    </form>
    {{if .error}}
    <div class="alert alert-danger">{{.error}}</div>
    {{end}}
  </div>
{{end}}
</div>
