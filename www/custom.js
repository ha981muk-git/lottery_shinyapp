$(document).ready(function() {
  const appConfig = window.liAppConfig || {};
  const ga4MeasurementId = (appConfig.ga4MeasurementId || '').trim();
  const consentStorageKey = 'li_analytics_consent';
  let analyticsConsent = false;
  let gaInitialized = false;
  const firedScrollDepths = new Set();

  const getConsent = () => {
    try {
      return window.localStorage.getItem(consentStorageKey);
    } catch (err) {
      return null;
    }
  };

  const setConsent = (value) => {
    try {
      window.localStorage.setItem(consentStorageKey, value);
    } catch (err) {
      // no-op
    }
  };

  const ensureDataLayer = () => {
    window.dataLayer = window.dataLayer || [];
    if (typeof window.gtag !== 'function') {
      window.gtag = function(){ window.dataLayer.push(arguments); };
    }
  };

  const ensureGtagScript = () => {
    if (!ga4MeasurementId) return;
    const encodedId = encodeURIComponent(ga4MeasurementId);
    const selector = `script[src*="googletagmanager.com/gtag/js?id=${encodedId}"]`;
    if (document.querySelector(selector)) return;

    const script = document.createElement('script');
    script.async = true;
    script.src = `https://www.googletagmanager.com/gtag/js?id=${encodedId}`;
    document.head.appendChild(script);
  };

  const initGA4 = () => {
    if (!analyticsConsent || gaInitialized || !ga4MeasurementId) return;
    ensureDataLayer();

    ensureGtagScript();
    window.gtag('consent', 'update', { analytics_storage: 'granted' });

    window.gtag('js', new Date());
    window.gtag('config', ga4MeasurementId, {
      anonymize_ip: true,
      send_page_view: true,
      page_language: appConfig.lang || 'de'
    });

    gaInitialized = true;
  };

  const trackEvent = (eventName, params = {}) => {
    if (!analyticsConsent) return;
    ensureDataLayer();
    initGA4();

    if (typeof window.gtag === 'function') {
      window.gtag('event', eventName, {
        event_category: 'engagement',
        ...params
      });
    }
  };

  const showConsentBanner = () => {
    const banner = document.getElementById('consentBanner');
    if (!banner) return;
    banner.classList.add('is-visible');
  };

  const hideConsentBanner = () => {
    const banner = document.getElementById('consentBanner');
    if (!banner) return;
    banner.classList.remove('is-visible');
  };

  const consentState = getConsent();
  if (consentState === 'accepted') {
    analyticsConsent = true;
    initGA4();
  } else if (consentState !== 'rejected') {
    showConsentBanner();
  }

  const acceptBtn = document.getElementById('consentAccept');
  if (acceptBtn) {
    acceptBtn.addEventListener('click', () => {
      analyticsConsent = true;
      setConsent('accepted');
      hideConsentBanner();
      initGA4();
      trackEvent('analytics_consent_accepted');
      trackEvent('session_start', { source: 'consent_accept' });
    });
  }

  const rejectBtn = document.getElementById('consentReject');
  if (rejectBtn) {
    rejectBtn.addEventListener('click', () => {
      analyticsConsent = false;
      setConsent('rejected');
      hideConsentBanner();
      if (typeof window.gtag === 'function') {
        window.gtag('consent', 'update', { analytics_storage: 'denied' });
      }
    });
  }

  if (consentState === 'accepted') {
    trackEvent('session_start', { source: 'stored_consent' });
  }

  document.addEventListener('visibilitychange', () => {
    if (!document.hidden) {
      trackEvent('visibility_return');
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

  const toAbsoluteLink = (rawLink) => {
    const normalized = String(rawLink || '').trim();
    if (!normalized) return '';

    try {
      return new URL(normalized).toString();
    } catch (err) {
      // Continue with relative/query resolution below.
    }

    try {
      return new URL(normalized, `${window.location.origin}${window.location.pathname}`).toString();
    } catch (err) {
      return normalized;
    }
  };

  if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
    window.Shiny.addCustomMessageHandler('copyViewLink', (payload) => {
      const link = toAbsoluteLink(payload && payload.url ? payload.url : '');
      const successText = (payload && payload.success) || 'Link copied.';
      const failureText = (payload && payload.failure) || 'Unable to copy link.';
      if (!link) {
        showFloatingToast(failureText, 'error');
        return;
      }

      if (navigator.clipboard && typeof navigator.clipboard.writeText === 'function') {
        navigator.clipboard.writeText(link)
          .then(() => {
            showFloatingToast(successText, 'success');
            trackEvent('copy_view_link', { method: 'clipboard_api' });
          })
          .catch(() => {
            const copied = fallbackCopyText(link);
            showFloatingToast(copied ? successText : failureText, copied ? 'success' : 'error');
            if (copied) {
              trackEvent('copy_view_link', { method: 'exec_command' });
            }
          });
      } else {
        const copied = fallbackCopyText(link);
        showFloatingToast(copied ? successText : failureText, copied ? 'success' : 'error');
        if (copied) {
          trackEvent('copy_view_link', { method: 'exec_command' });
        }
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
  const isEnglish = (appConfig.lang || '').toLowerCase() === 'en';
  const toggleLabel = isEnglish ? 'Menu' : 'Menü';
  const toggleAriaLabel = isEnglish ? 'Open analysis filters' : 'Analysefilter oeffnen';

  const toggleButton = $('<button class="sidebar-toggle-btn" type="button"></button>')
    .attr('aria-label', toggleAriaLabel)
    .attr('aria-expanded', 'false')
    .text(toggleLabel)
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
        const nextState = !open;
        layout.dataset.sidebarOpen = String(nextState);
        $(this).attr('aria-expanded', String(nextState));
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

  // Analytics event hooks
  document.addEventListener('click', (event) => {
    const eventNode = event.target.closest('[data-analytics-event]');
    if (eventNode) {
      trackEvent(eventNode.getAttribute('data-analytics-event'));
    }

    const anchor = event.target.closest('a[href]');
    if (!anchor) return;
    const href = anchor.getAttribute('href') || '';
    if (/^https?:\/\//i.test(href) || /^mailto:/i.test(href)) {
      trackEvent('outbound_click', { link_url: href.slice(0, 200) });
    }
  });

  document.addEventListener('shiny:inputchanged', (event) => {
    if (!event || !event.name) return;
    const key = event.name;

    if (key === 'inputs1-metric') {
      trackEvent('metric_switch', { metric: String(event.value || '') });
      return;
    }

    if (key === 'inputs1-range' || key === 'inputs1-dateRange') {
      trackEvent('filter_change', { filter_name: key.replace('inputs1-', '') });
      return;
    }

    if (key === 'inputs1-refresh') {
      trackEvent('refresh_click');
    }
  });

  const scrollMilestones = [25, 50, 75, 90];
  const onScroll = () => {
    const viewportHeight = window.innerHeight || document.documentElement.clientHeight || 0;
    const totalHeight = Math.max(document.body.scrollHeight, document.documentElement.scrollHeight);
    const maxScrollable = Math.max(totalHeight - viewportHeight, 1);
    const scrolled = window.scrollY || document.documentElement.scrollTop || 0;
    const pct = Math.min(100, Math.round((scrolled / maxScrollable) * 100));

    scrollMilestones.forEach((depth) => {
      if (pct >= depth && !firedScrollDepths.has(depth)) {
        firedScrollDepths.add(depth);
        trackEvent('scroll_depth', { depth_percent: depth });
      }
    });
  };

  window.addEventListener('scroll', onScroll, { passive: true });
});
