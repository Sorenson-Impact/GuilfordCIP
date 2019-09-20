$(document).ready(() => {
  // Fixes hover issue for classes
  // Removes Shiny's autogenerated
  $('.nav.nav-tabs>li:nth-of-type(2)>a:nth-of-type(1)').remove();
  $('.nav-tabs>li:nth-of-type(4)>a:nth-of-type(1)').remove();
  $('.nav-tabs>li:nth-of-type(6)>a:nth-of-type(1)').remove(); 
  $('.nav-tabs .btn .glyphicon.glyphicon-triangle-bottom').remove(); 

  
  // Snipe out the list dropdowns by hovering
  $('.nav.nav-tabs li:nth-of-type(2)>.sw-dropdown').hover(
    function(){ 
      $('.nav.nav-tabs li:nth-of-type(2) .sw-dropdown-content.animated').addClass('sw-show'); 
    },
    function(){ 
      $('.nav.nav-tabs li:nth-of-type(2) .sw-dropdown-content.animated').removeClass('sw-show');
    }
  );
  
  $('.nav.nav-tabs li:nth-of-type(4)>.sw-dropdown').hover(
    function(){ 
      $('.nav.nav-tabs li:nth-of-type(4) .sw-dropdown-content.animated').addClass('sw-show'); 
    },
    function(){ 
      $('.nav.nav-tabs li:nth-of-type(4) .sw-dropdown-content.animated').removeClass('sw-show');
    }
  );
  
  $('.nav.nav-tabs li:nth-of-type(6)>.sw-dropdown').hover(
    function(){ 
      $('.nav.nav-tabs li:nth-of-type(6) .sw-dropdown-content.animated').addClass('sw-show'); 
    },
    function(){ 
      $('.nav.nav-tabs li:nth-of-type(6) .sw-dropdown-content.animated').removeClass('sw-show');
    }
  );
});
