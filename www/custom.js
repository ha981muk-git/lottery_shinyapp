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

  const visitorPayload = () => ({ id: visitorToken, nonce: Date.now() });

  const publishVisitorToken = () => {
    if (!visitorToken) return false;
    if (window.Shiny && typeof window.Shiny.setInputValue === 'function') {
      window.Shiny.setInputValue('visitor_token', visitorPayload(), { priority: 'event' });
      return true;
    }
    return false;
  };

  let publishAttempts = 0;
  const ensureVisitorTokenPublished = () => {
    if (publishVisitorToken()) return;
    publishAttempts += 1;
    if (publishAttempts <= 20) {
      setTimeout(ensureVisitorTokenPublished, 250);
    }
  };

  ensureVisitorTokenPublished();
  document.addEventListener('shiny:connected', () => {
    publishVisitorToken();
    setTimeout(publishVisitorToken, 300);
    setTimeout(publishVisitorToken, 1200);
  }, { once: true });

  document.addEventListener('visibilitychange', () => {
    if (!document.hidden) {
      publishVisitorToken();
    }
  });

  // Lightweight visual mode for lower-end devices.
  const lowEndDevice =
    (navigator.hardwareConcurrency && navigator.hardwareConcurrency <= 4) ||
    (navigator.deviceMemory && navigator.deviceMemory <= 4);
  if (lowEndDevice) {
    document.documentElement.classList.add('low-end-device');
  }

  const showFloatingToast = (message, type = 'success') => {
    if (!message) return;
    const toast = document.createElement('div');
    toast.className = `floating-toast ${type}`;
    toast.textContent = message;
    document.body.appendChild(toast);

    window.requestAnimationFrame(() => {
      toast.classList.add('show');
    });

    setTimeout(() => {
      toast.classList.remove('show');
      setTimeout(() => {
        if (toast.parentNode) {
          toast.parentNode.removeChild(toast);
        }
      }, 260);
    }, 2400);
  };

  const fallbackCopyText = (text) => {
    const helper = document.createElement('textarea');
    helper.value = text;
    helper.setAttribute('readonly', 'readonly');
    helper.style.position = 'fixed';
    helper.style.opacity = '0';
    helper.style.pointerEvents = 'none';
    document.body.appendChild(helper);
    helper.select();
    helper.setSelectionRange(0, helper.value.length);

    let copied = false;
    try {
      copied = document.execCommand('copy');
    } catch (err) {
      copied = false;
    }

    document.body.removeChild(helper);
    return copied;
  };

  if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
    window.Shiny.addCustomMessageHandler('copyViewLink', (payload) => {
      const link = payload && payload.url ? payload.url : '';
      const successText = (payload && payload.success) || 'Link copied.';
      const failureText = (payload && payload.failure) || 'Unable to copy link.';
      if (!link) {
        showFloatingToast(failureText, 'error');
        return;
      }

      if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
        navigator.clipboard.writeText(link)
          .then(() => showFloatingToast(successText, 'success'))
          .catch(() => {
            const copied = fallbackCopyText(link);
            showFloatingToast(copied ? successText : failureText, copied ? 'success' : 'error');
          });
      } else {
        const copied = fallbackCopyText(link);
        showFloatingToast(copied ? successText : failureText, copied ? 'success' : 'error');
      }
    });
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
    $('.bslib-sidebar-layout > .collapse-toggle').css('display', 'none');
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
