<template>
    <div>
        <p>User: {{ username }}</p>
    </div>
</template>
  
<script>
import Vue from 'vue'

export default {
    data: function() {
        return {
            username: "",
        }
    },
    async mounted() {
        this.username = await this.$axios.$get("/api/user/me") || "";
    },
}
</script>