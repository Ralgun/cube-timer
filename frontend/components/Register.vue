<template>
    <div>
        <input v-model="username" placeholder="Username">
        <input type="password" v-model="password"  placeholder="Password">
        <button @click="register">Register</button>
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
        register: function() {
            this.$axios.$post("/api/create-user", { uUsername: this.username, uPassword: this.password });
        }
    }
}
</script>