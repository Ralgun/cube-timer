<template>
  <div>
    <Stopwatch />
    <Register />
    <Login />
    <ProfileStatus />
  </div>
</template>

<script lang="ts">
import Vue from 'vue'
import Stopwatch from "../components/Stopwatch.vue"

export default Vue.extend({
  name: 'IndexPage',

  components: {
    Stopwatch
  },

  data: () => ({
    hello_world: "",
  }),

  methods:{
  }, 
})
</script>
