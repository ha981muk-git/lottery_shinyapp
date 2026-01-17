$(document).ready(function() {
  // --- Sidebar Overlay Fix ---
  let debounceTimer;
  function fixSidebarOverlay() {
    $('.bslib-sidebar-layout > .main').css({
      'opacity': '1',
      'filter': 'none',
      'pointer-events': 'auto',
      'transition': 'none'
    });
    $('.sidebar-backdrop, .bslib-sidebar-backdrop').remove();
    $('.bslib-sidebar-layout').css({
      'display': 'grid',
      'grid-template-columns': 'auto 1fr',
      'gap': '20px'
    });
  }
  fixSidebarOverlay();
  setTimeout(fixSidebarOverlay, 100);
  setTimeout(fixSidebarOverlay, 500);
  
  // Debounced observer
  const observer = new MutationObserver(() => {
    clearTimeout(debounceTimer);
    debounceTimer = setTimeout(fixSidebarOverlay, 100);
  });
  observer.observe(document.body, { childList: true, subtree: true });
  
  // CRITICAL: Stop observing after 3 seconds - sidebar is stable
  setTimeout(() => observer.disconnect(), 3000);

  // --- Responsive Sidebar Toggle ---
  const toggleButton = $('<button class="sidebar-toggle-btn">☰ Menü</button>')
    .css({
      position: 'fixed',
      top: '15px',
      left: '15px',
      background: '#8b5cf6',
      color: '#fff',
      border: 'none',
      padding: '10px 14px',
      borderRadius: '8px',
      fontSize: '18px',
      cursor: 'pointer',
      zIndex: 2000,
      display: 'none'
    })
    .appendTo('body')
    .on('click', function() {
      const layout = document.querySelector('.bslib-sidebar-layout');
      if (layout) {
        const open = layout.dataset.sidebarOpen === 'true';
        layout.dataset.sidebarOpen = !open;
      }
    });

  function checkScreen() {
    if (window.innerWidth < 768) {
      toggleButton.show();
    } else {
      toggleButton.hide();
    }
  }
  checkScreen();
  $(window).on('resize', checkScreen);
});