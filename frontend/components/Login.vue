<template>
  <div class="modal">
    <p v-if="error" class="error">{{ error }}</p>
    <input v-model="username" placeholder="Username"> <br>
    <input type="password" v-model="password"  placeholder="Password"> <br>
    <button @click="login" id="finish">Login</button>
  </div>
</template>
  
<script>
import Vue from 'vue'

export default {
    data: function () {
        return {
            username: "",
            password: "",
            error: "",
        };
    },
    methods: {
        login: async function () {
            try {
                await this.$axios.$post("/api/login", { username: this.username, password: this.password });
                window.location.reload(true);
            }
            catch (err) {
                if (err.response.status === 401) {
                    this.error = "Failed to login";
                }
                else {
                    this.error = "Unknown error, try logging in later";
                }
            }
        }
    },
}
</script>

<style scoped >
@import "styles/modal_style.css";
</style>