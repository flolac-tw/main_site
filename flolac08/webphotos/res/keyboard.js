<!--

/*
************************************************************

  Note: Original routine taken from BluPlusPlus 2 see:
  http://www.datadosen.se/jalbum/forum/thread.jspa?threadID=1458

  ExhibitPlus: Keyboard support
    Keys:
      RIGHT arrow  	: next page
      LEFT arrow   	: previous page
      // Page UP     	: index page (no longer used)
      HOME         	: first page
      END          	: last page
      S,s          	: toggle start/stop slide show
      I,i          	: toggle exif info (when appropriate)
      Z,z			: zoom image (when appropriate)	
    Tested on: IE6, Mozzila 1.4, Netscape 7.1, Opera 7.2
    Bugs:
     - HOME,END, NUM+, NUM-  doesn't work in Opera
************************************************************
*/

var takenAction = false;

//IE&Gecko Code
function IEGeckoKeyPress(oEvent) {
	if (!oEvent) var oEvent = window.event;
	if (oEvent.keyCode) myKeyCode = oEvent.keyCode;
	else if (oEvent.which) myKeyCode = oEvent.which;
	if (oEvent.repeat || takenAction) {	return;	}
	if (myKeyCode >= 16 && myKeyCode <= 18) { return; }
	if (oEvent.shiftKey) { myKeyCode += 1000; }
	if (oEvent.ctrlKey)  { myKeyCode += 2000; }
	if (oEvent.altKey)   { myKeyCode += 4000; }
	//alert(oEvent.type + "=" + myKeyCode);
	myKeyPress(myKeyCode);
}

function myKeyPress(myKeyCode) {
	switch (myKeyCode) {
		case 39:					// RIGHT arrow
		case 107:					// NUM +
			takenAction = true;
			nextPage();
			break;
		case 37:					// LEFT arrow
		case 109:					// NUM -
			takenAction = true; 
			prevPage();
			break;
//		case 33: 					// Page UP
//			takenAction = true;
//			indexPage();
//			break;
		case 36:					// HOME
			takenAction = true;
			firstPage();
			break;
		case 35:					// END
			takenAction = true;
			lastPage();
			break;
		case 83:					// S,s
//		case 27:					// ESC
			if ( isSlideShow ) {
				// Only if i have slide show
				takenAction = true;
				navSlideShow();
			}
			break;
		case 73:					// I,i
			takenAction = true;
			navToggleInfo();
			break;
//		case 34:					// Page DOWN
		case 90:					// Z,z
			takenAction = true;
			zoom();
			break;
		default:	 
			//alert(oEvent.type + "=" + myKeyCode);
			break;
		}
}

function initKeyboard() {
	document.onkeydown = IEGeckoKeyPress;
}

-->