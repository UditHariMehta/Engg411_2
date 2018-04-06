var quill = new Quill('#editor-container', {
  modules: {
    toolbar: [
      [{ header: [1, 2, false] }],
      ['bold', 'italic', 'underline'],
      ['image', 'code-block']
    ]
  },
  placeholder: 'Compose an epic...',
  theme: 'snow'  // or 'bubble'
});

$(('#submit').on('click',function(){
  var wordData =  textLineData.createNode(word, this.reasonerMode);
  $.ajax({
    type:'POST',
    url:"/peng",
    data:wordData,
    success:function(){
      console.log("success");
    },
    error: function ()
                {
                      alert("Failed JSON object input when loading file: \n "+errorThrown);
                }









  })
}


var KeyHandler = {
      keyUpdate: function(d, e) {
            var keyID = e.keyCode == 13 ? e.keyCode : e.charCode;
            console.log(keyID)
            var keyVal = (String.fromCharCode(keyID));
            var charAllowed = (keyID != 13 ) || keyID === 13;
            if(keyVal == " " && viewModel.textAreaStr().length == 0)
                  return false;
            if(!charAllowed)
                  return false;
            if(viewModel.allowInput) {
                  this.switchKeyVal(keyVal);
                  keyVal = (keyID == 13) ? "" : keyVal;
                  viewModel.textAreaStr(viewModel.textAreaStr()+keyVal);

            }
            else {
                  var word = textLineData.nodes[textLineData.nodes.length-1];
                  alert('"'+word+'" is not a valid token. Please go back and re-enter a token or add to lexicon via predictive table');
                  return false;
            }

            return true;
      },

      enterKey: function() {
        // changes made here
          //  var i =0;
            //var isEndOfSentence = viewModel.textAreaStr().charAt(viewModel.textAreaStr().length === " ");
           var isEndOfSentence = viewModel.textAreaStr().charAt(viewModel.textAreaStr().length-1) == "." || viewModel.textAreaStr().charAt(viewModel.textAreaStr().length-1) == "?";


            if(isEndOfSentence) {
                  var submitStr = viewModel.textAreaStr();
                  //.replace(" .", ".");
                  //submitStr = submitStr.replace(" ?", "?");
                  textLineData.addSentence(submitStr);
                  viewModel.textList.push(submitStr);
                  viewModel.textAreaStr('');
                  viewModel.token('');
                  viewModel.$text_field.val('');
                  viewModel.init();

          }
      },

      punctuation: function(chr) {
            var sizeOfWord = viewModel.token().length;
            var prevWord = viewModel.token().slice(0, sizeOfWord);
            if(prevWord != "") {
                  viewModel.postToken(viewModel.token().slice(0, sizeOfWord));
            }
            viewModel.firstIndexOfCurrentWord = viewModel.textAreaStr().length ;
            if (chr == '.' || chr == '?') {
                  if(  $.inArray(chr, viewModel.lookUpTable()) != -1)
                        viewModel.postToken(chr);
                  else {
                        textLineData.sposNum--;
                        viewModel.allowInput = false;
                  }
            }
      },






      switchKeyVal: function(keyVal) {
            switch(keyVal) {
                  case ',':
                        KeyHandler.punctuation('');
                        viewModel.token(",");
                        break;
                  case ' ': //SPACE CLICKED
                        KeyHandler.punctuation('');
                        viewModel.token('');
                        break;
                  case '.':
                        KeyHandler.punctuation(keyVal);
                        viewModel.token(viewModel.token()+keyVal);
                        break;
                  case '?':
                        KeyHandler.punctuation(keyVal);
                        viewModel.token(viewModel.token()+keyVal);
                        break;
                  case String.fromCharCode(13):  // Enter
                        KeyHandler.enterKey();
                        break;
                  default:
                        viewModel.token(viewModel.token()+keyVal);
            }
      },



}
