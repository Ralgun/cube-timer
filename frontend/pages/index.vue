<template>
  <div id="main">
    <div class="box">
      <div class="row header">
        <div class="flex-container-top">
          <button @click="showLoginModal = true">Login</button>
          <button @click="showRegisterModal = true">Register</button>
          <ProfileStatus />
        </div>
      </div>
      <div class="row content">
        <div class="main-columns">
          <div class="main-child-left"><p>Test</p></div>
          <div class="main-child-right">
            <div class="top-spacer right-child-content" />
            <Stopwatch class="right-child-content" id="stopwatch" />
          </div>
        </div>
      </div>
      <div class="row footer">
        <div class="flex-container-top">
          <p><b>footer</b> (fixed height)</p>
          <img src="/badges/nonbinary.png" alt="non-binary pride" title="enby!">
        </div>
      </div>
      <Modal v-show="showLoginModal" @close="test" ><Login @close="test" /></Modal>
      <Modal v-show="showRegisterModal" @close="test" ><Register @close="test"/></Modal>
    </div>
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
    showLoginModal: false,
    showRegisterModal: false,
  }),

  methods:{
    test: function() {
      this.showLoginModal = false;
      this.showRegisterModal = false;
      console.log("Logged");
    },
  }, 
})
</script>

<style>

.main-columns {
  height: 100%;
  display: flex;
  flex-direction: row;
}

.main-child-left {
  height: 100%;
  width: 15%;
  background-color: #C3B1E1;
}

.main-child-right {
  height: 100%;
  width: 85%;
}

.right-child-content {
  width: 100%;
  text-align: center;
}

.top-spacer {
  height: 30%;
}

#flex-container-main {
  display: flex;
  flex-direction: row;
  width: 100%;
  height: 95%;
}

.flex-container-top {
  display: flex;
  flex-direction: row;
  width: 100%;
  height: 100%;
}


html,
body {
  height: 100%;
  margin: 0;
}

body > div {
  height: 100%;
  margin: 0;
}

body > div > div {
  height: 100%;
  margin: 0;
}

#main {
  height: 100%;
  margin: 0;
}

.box {
  display: flex;
  flex-flow: column;
  height: 100%;
}

.box .row {
  border: 1px dotted grey;
}

.box .row.header {
  flex: 0 1 auto;
  /* The above is shorthand for:
  flex-grow: 0,
  flex-shrink: 1,
  flex-basis: auto
  */
}

.box .row.content {
  flex: 1 1 auto;
}

.box .row.footer {
  flex: 0 1 40px;
}

</style>