<template>
    <div class="modal">
        <input v-model="username" placeholder="Username">
        <input type="password" v-model="password"  placeholder="Password">
        <button @click="register" id="finish">Register</button>
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
        register: async function() {
            this.$axios.$post("/api/register", { username: this.username, password: this.password });
            this.$emit("close");
        }
    }
}
</script>

<style scoped >
@import "styles/modal_style.css";
</style>
