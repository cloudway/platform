<div id="confirm-modal" class="modal" role="dialog">
  <div class="modal-dialog" role="document">
    <div class="modal-content">
      <div class="modal-header modal-danger">
        <button type="button" class="close" data-dismiss="modal">&times;</button>
        <h4 class="modal-title">警告</h4>
      </div>
      <div class="modal-body">
        <p id="confirm-message"></p>
      </div>
      <div class="modal-footer">
        <button type="button" class="btn btn-danger" data-dismiss="modal" id="confirm-button">继续</button>
        <button type="button" class="btn btn-default" data-dismiss="modal">取消</button>
      </div>
    </div>
  </div>
</div>

<script>
$('#confirm-modal').on('show.bs.modal', function(event) {
  var button = $(event.relatedTarget)
  var form = button.closest('form')
  $('#confirm-message').text(button.data('message'))
  $('#confirm-button').on('click', function(e) {
    form.trigger('submit')
  })
})
</script>
