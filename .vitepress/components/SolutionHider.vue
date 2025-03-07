<script setup>
    import { onMounted } from 'vue'
    import { useRoute } from 'vitepress'

    // time when last lab in the first week ends
    const SHOW_FIRST_SOLS = new Date('2025-02-20T18:00:00+01:00').getTime();

    const route = useRoute()

    onMounted(() => { handlePageChange() })

    function hide_details(reveal_date) {
        const detailsElements = document.querySelectorAll('details[hideme]');
        for (const details of detailsElements) {
            const summary = details.querySelector('summary');
            const summaryContent = summary ? summary.outerHTML : '';
            details.innerHTML = `${summaryContent} Solutions will be revealed after labs on ${reveal_date}.`;
        }
    }
    function handlePageChange() {
        const now = new Date(Date.now()).getTime();
        const page_week_no = parseInt(route.path.match(/\d+/)[0]) - 1;
        const millis_in_day = 24 * 60 * 60 * 1000;
        const reveal_at = SHOW_FIRST_SOLS + 7 * page_week_no * millis_in_day;
        const days_left = (reveal_at - now) / millis_in_day;
        const should_reveal = days_left <= 0;

        if (!should_reveal) {
            hide_details(new Date(reveal_at).toLocaleDateString());
        }
    }
</script>

<template>
</template>
