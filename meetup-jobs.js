

function handleFileSelect(filelist) {
    console.log("in handleFileSelect")
    var file = filelist[0];
    console.log(file)
    var reader = new FileReader();
    
    reader.onload = (function(theFile) {
        return function(e) {
            console.log("in reader.onload callback") 
            console.log(e.target.result)
            var data = $.csv.toObjects(e.target.result);
            var html = generateTable(data);
            $('#result3').empty();
            $('#result3').html(html);
        };
      })(file);

    reader.readAsText(file)
}

// build HTML table data from an array (one or two dimensional)
function generateTable(data) {
    var html = '';

    if(typeof(data[0]) === 'undefined') {
      return null;
    }

    if(data[0].constructor === String) {
      html += '<tr>\r\n';
      for(var item in data) {
        html += '<td>' + data[item] + '</td>\r\n';
      }
      html += '</tr>\r\n';
    }

    if(data[0].constructor === Array) {
      for(var row in data) {
        html += '<tr>\r\n';
        for(var item in data[row]) {
          html += '<td>' + data[row][item] + '</td>\r\n';
        }
        html += '</tr>\r\n';
      }
    }

    if(data[0].constructor === Object) {
      for(var row in data) {
        html += '<tr>\r\n';
        for(var item in data[row]) {
          html += '<td>' + item + ':' + data[row][item] + '</td>\r\n';
        }
        html += '</tr>\r\n';
      }
    }

    return html;
}



/**
     * This function is called the first time that the Realtime model is created
     * for a file. This function should be used to initialize any values of the
     * model. In this case, we just create the single string model that will be
     * used to control our text box. The string has a starting value of 'Hello
     * Realtime World!', and is named 'text'.
     * @param model {gapi.drive.realtime.Model} the Realtime root model object.
     */
function initializeModel(model) {
  var string = model.createString('Hello Realtime World!');
  model.getRoot().set('text', string);
}

/**
 * This function is called when the Realtime file has been loaded. It should
 * be used to initialize any user interface components and event handlers
 * depending on the Realtime model. In this case, create a text control binder
 * and bind it to our string model that we created in initializeModel.
 * @param doc {gapi.drive.realtime.Document} the Realtime document.
 */


function onFileLoaded(doc) {
  var string = doc.getModel().getRoot().get('text');

  // Keeping one box updated with a String binder.
  var textArea1 = document.getElementById('editor1');
  gapi.drive.realtime.databinding.bindString(string, textArea1);

  // Keeping one box updated with a custom EventListener.
  var textArea2 = document.getElementById('editor2');
  var updateTextArea2 = function(e) {
    textArea2.value = string;
  };
  string.addEventListener(gapi.drive.realtime.EventType.TEXT_INSERTED, updateTextArea2);
  string.addEventListener(gapi.drive.realtime.EventType.TEXT_DELETED, updateTextArea2);
  textArea2.onkeyup = function() {
    string.setText(textArea2.value);
  };
  updateTextArea2();

  // Enabling UI Elements.
  textArea1.disabled = false;
  textArea2.disabled = false;
}

/**
 * Options for the Realtime loader.
 */
var realtimeOptions = {
  /**
   * Client ID from the APIs Console.
   */
  clientId: '157211028949.apps.googleusercontent.com',

  /**
   * The ID of the button to click to authorize. Must be a DOM element ID.
   */
  authButtonElementId: 'authorizeButton',

  /**
   * Function to be called when a Realtime model is first created.
   */
  initializeModel: initializeModel,

  /**
   * Autocreate files right after auth automatically.
   */
  autoCreate: true,

  /**
   * Autocreate files right after auth automatically.
   */
   defaultTitle: "New Realtime Quickstart File",

  /**
   * Function to be called every time a Realtime file is loaded.
   */
  onFileLoaded: onFileLoaded
}

/**
 * Start the Realtime loader with the options.
 */
function startMeetupJobs() {

    // load the CSV file from the server
    //getAnnouncements("meetup-jobs.csv");
  
    // set up the auth stuff for Realtime
    /* var realtimeLoader = new rtclient.RealtimeLoader(realtimeOptions);
    realtimeLoader.start();
*/
}