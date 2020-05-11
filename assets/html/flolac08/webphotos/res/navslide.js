<!--
//// JavaScript Navigation for slide
// Variables from setup.js: nextPageUrl, prevPageUrl, indexPageUrl, firstPageUrl, lastPageUrl
function nextPage() {
	document.location=nextPageUrl;
	takenAction = false;
}
function prevPage() {
	document.location=prevPageUrl;
	takenAction = false;
}
function indexPage() {
	document.location=indexPageUrl;
	takenAction = false;
}
function firstPage() {
	document.location=firstPageUrl;
	takenAction = false;
}
function lastPage() {
	document.location=lastPageUrl;
	takenAction = false;
}
function zoom() {
	document.location=zoomPageUrl;
	takenAction = false;
}
function navSlideShow() {
	if (readCookie('slideShow') == 'true') {
		stopSlideShow();
		document.location.reload(false); 
	} else {
		setSlideShowCookie();
		nextPage();
	}
	takenAction = false;
}
function navToggleInfo() {
	toggleEXIFInfo();
	takenAction = false;
}
-->