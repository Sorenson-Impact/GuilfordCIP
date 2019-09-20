$(document).ready(() => {
  // Handles redirection and highlights classes 
  let url = location.href.replace(/\/$/, "");
  if (location.hash) {
    const hash = url.split("#");
    $('.sidebar-menu a[href="#' + hash[1] + '"]').tab("show");
    url = location.href.replace(/\/#/, "#");
    history.replaceState(null, null, url);
    setTimeout(() => {
      $(window).scrollTop(0);
    }, 400);
  }

  $('a[data-toggle="tab"]').on("click", function() {
    let newUrl;
    const hash = $(this).attr("href");
    if (hash == "#home") {
      newUrl = url.split("#")[0];
    } else {
      newUrl = url.split("#")[0] + hash;
    }
    newUrl += "/";
    history.replaceState(null, null, newUrl);
  });

  $(".overview-icons a").click(function() {
    $(".sidebar-menu .active").removeClass("active");
    $(".sidebar-menu [href='" + this.hash + "']")
      .parent()
      .addClass("active");
  });
});
