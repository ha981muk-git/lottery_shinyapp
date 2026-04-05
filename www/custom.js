$(document).ready(function() {
  // Persistent anonymous visitor token for real daily unique-visitor counting.
  const makeVisitorToken = () => {
    if (window.crypto && typeof window.crypto.randomUUID === 'function') {
      return `v_${window.crypto.randomUUID().replace(/-/g, '')}`;
    }
    return `v_${Date.now().toString(36)}_${Math.random().toString(36).slice(2, 12)}`;
  };

  let visitorToken = null;
  try {
    visitorToken = window.localStorage.getItem('li_visitor_token');
    if (!visitorToken) {
      visitorToken = makeVisitorToken();
      window.localStorage.setItem('li_visitor_token', visitorToken);
    }
  } catch (err) {
    visitorToken = makeVisitorToken();
  }

  const publishVisitorToken = () => {
    if (!visitorToken) return;
    if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
      window.Shiny.setInputValue('visitor_token', visitorToken, { priority: 'event' });
    }
  };

  publishVisitorToken();
  document.addEventListener('shiny:connected', publishVisitorToken, { once: true });

  // Lightweight visual mode for lower-end devices.
  const lowEndDevice =
    (navigator.hardwareConcurrency && navigator.hardwareConcurrency <= 4) ||
    (navigator.deviceMemory && navigator.deviceMemory <= 4);
  if (lowEndDevice) {
    document.documentElement.classList.add('low-end-device');
  }

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

  // Smooth-scroll in-page nav anchors.
  $('a[href^="#"]').on('click', function(event) {
    const href = $(this).attr('href');
    if (!href || href === '#') return;
    const target = $(href);
    if (!target.length) return;

    event.preventDefault();
    $('html, body').animate({
      scrollTop: Math.max(0, target.offset().top - 88)
    }, 420);
  });

  // Share button: native share on supported devices, else copy link.
  $('.share-app-btn').on('click', async function() {
    const button = this;
    const shareLabel = button.dataset.shareLabel || 'Share Analysis';
    const copiedLabel = button.dataset.copiedLabel || 'Link Copied';
    const fallbackLabel = button.dataset.fallbackLabel || 'Copy this link manually';
    const currentUrl = window.location.href;

    if (navigator.share) {
      try {
        await navigator.share({ title: document.title, url: currentUrl });
        return;
      } catch (err) {
        // Fall back to clipboard if sharing is cancelled or unavailable.
      }
    }

    if (navigator.clipboard && window.isSecureContext) {
      try {
        await navigator.clipboard.writeText(currentUrl);
        button.textContent = copiedLabel;
        setTimeout(() => {
          button.textContent = shareLabel;
        }, 1600);
        return;
      } catch (err) {
        // Fall through to prompt fallback.
      }
    }

    window.prompt(fallbackLabel, currentUrl);
  });

  // Reveal cards on first viewport entry for a cleaner staged load.
  const revealTargets = document.querySelectorAll('.metric-card, .content-card, .value-box-custom');
  if ('IntersectionObserver' in window && revealTargets.length) {
    const revealObserver = new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        if (!entry.isIntersecting) return;
        entry.target.classList.add('is-visible');
        revealObserver.unobserve(entry.target);
      });
    }, { threshold: 0.12 });

    revealTargets.forEach((el) => {
      el.classList.add('will-reveal');
      revealObserver.observe(el);
    });
  }
});
