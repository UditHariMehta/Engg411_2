/**
*   Based on the work by Rudz Evan Bernardez, Macquarie University
*   File Name: ViewModel2.js
*
*/

var viewModel = {
      $text_field: $("#text_field"),
      textAreaStr: ko.observable(""), // For dispaly
      result: ko.observable(""),
      firstIndexOfCurrentWord: 0,

      token: ko.observable(""),
      textList: ko.observableArray([]),
      smallAsp: "",
      bigAsp: "",
      aspState: false,

      $procSpan: $("#proc-span"),

      allowInit: true,
      // CLASSES
      lookahead: Lookahead,
      allowInput: true,

      catTable: ko.observableArray([]),
      anaExp: ko.observableArray([]),

      lookaheadObject: ko.observableArray([]),

      reasonerMode: "normal", // default settings
      inputMode: ko.observable("Text Mode"), //default settings
      speechData: ko.observable(""),

      $loader: $(".loader"),


      textParaList: ko.observableArray([]),


      submitButton: function() {
            KeyHandler.enterKey();

      },

      changeToNormal: function() {
            this.reasonerMode = "normal"
      },

      changeToBrave: function() {
            this.reasonerMode = "brave";
      },

      changeToCautious: function() {
            this.reasonerMode = "cautious";
      },

      loadLookahead: function() {
            this.init();  // only needed at first load
            var lookaheadTable = this.lookahead.createLookaheadTable(this.lookahead);
            this.lookaheadObject(lookaheadTable);
      },

      loadLookahead2: function() {
            var lookaheadTable = this.lookahead.createLookaheadTable(this.lookahead);
            this.lookaheadObject(lookaheadTable);
      },

      postWordClicked: function(data, event) {
            clickHelper.postWordClicked(data, event);
      },

      postAnaExpClicked: function() {
            clickHelper.postAnaExpClicked(this);
      },

      lookUpTable: ko.observableArray([]),
      asp: ko.observable(''),
      answer: ko.observable(''),
      currentInitialLookUpTable: [],
      initSentenceLookUp: [],

      asyncFlag: false,

      fileNames: ko.observableArray([]),
      //WIDGETS





      init: function() {
            var lastNodePostedWasBlank = textLineData.nodes[textLineData.nodes.length-1] != " ";
            var textAreaEmpty = this.textAreaStr().length == 0
            if (textAreaEmpty && lastNodePostedWasBlank) {
                  this.postToken(" ");
                  this.initSentenceLookUp = this.lookUpTable();
                  this.initLookUpObj = this.lookaheadObject();
            }
            else if (this.allowInput){
                  this.$text_field.val(this.textAreaStr());
            }
      },



      postToken2(word) {
            var wordData =  textLineData.createNode(word, this.reasonerMode);
            var request = $.ajax({
                  url : "/peng/",
                  type: "POST",
                  data : wordData,
                  async: this.asyncFlag
            });
            return request;
      },




    



contains: function(target, arr) {
      for (i = 0; i < arr.length; i++) {
            if (target == arr[i]) {
                  return true;
            }
      }
      return false;
},


keyHandler: function(d, e) {
      return KeyHandler.keyUpdate(d, e);
},

//fullStopHandler: function(keyVal)
//{
  //KeyHandler.punctuation(keyVal);
  //viewModel.token(viewModel.token()+keyVal);
//},

backSpaceHandler: function(d, e) {
      KeyHandler.backspace(d, e);
},

createJsonObject: function(emode, token, fname, stext, reason, rmode) {
      var object = {
            id: -1,
            inputmode:"text",
            editmode:emode,
            token: token,
            nbest:"[]",
            featurestructure:"{ \"cat\" : \" \",  \"wfm\" : \" \"}",
            filename: fname,
            spectext: stext,
            snum: -1,
            spos: -1,
            reasoner: reason,
            reasonermode: rmode
      }
      return object;
}


};


}



}











/// for asp toggle (dev mode)
$("[name='my-checkbox']").bootstrapSwitch();
$('input[name="my-checkbox"]').on('switchChange.bootstrapSwitch', function(event, state) {
      if (state) {
            viewModel.asp(viewModel.bigAsp);
      }
      else {
            viewModel.asp(viewModel.smallAsp);
      }
      viewModel.aspState = state;
});

});
