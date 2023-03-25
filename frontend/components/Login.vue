<template>
    <div>
        <input v-model="username" placeholder="Username">
        <input type="password" v-model="password"  placeholder="Password">
        <button @click="login">Login</button>
    </div>
</template>
  
<script>
import Vue from 'vue'

export default {
    data: function() {
        return {
            username: "",
            password: "",
        }
    },
    methods: {
        login: async function() {
            await this.$axios.$post("/api/login", { uUsername: this.username, uPassword: this.password });
            window.location.reload(true);
        }
    }
}
</script>