<!--
//// JavaScript Navigation for slide
// Variables from setup.js: nextPageUrl, prevPageUrl, indexPageUrl, firstPageUrl, lastPageUrl
function nextPage() {
	//Never have slide show in originalslide2
	takenAction = false;
}
function prevPage() {
	//Never have slide show in originalslide2
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
	//Never have slide show in originalslide2
	takenAction = false;
}
function navToggleInfo() {
	//Never have show info in originalslide2
	takenAction = false;
}
-->