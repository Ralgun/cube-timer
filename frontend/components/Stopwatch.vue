<template>
  <div class="main">
      <span id="secs">{{ disp_secs }}</span> <span id="millis">{{ disp_millis }}</span>
  </div>
</template>

<script>
import Vue from 'vue'

let timer;
let running = false;

export default {
  data: () => ({
    millis: 0,
    disp_secs: "0",
    disp_millis: "00",
  }),

  methods: {
    start: function() {
      clearInterval(timer);
      console.log("Start");
      running = true;
      
      this.millis = 0;

      timer = setInterval(() => {
        this.millis += 10;
        this.disp_secs = Math.floor(this.millis/1000);
        let temp = Math.floor((this.millis % 1000)/10);
        if (temp < 10) {
          this.disp_millis = `0${temp}`;
        } else {
          this.disp_millis = temp;
        }
      }, 10);
    },
    stop: function() {
      clearInterval(timer);
      console.log("stop");
      running = false;
      
      const formData= new FormData();
      formData.set("millis", this.millis);
      this.$axios.$post("/api/send-time", { tMillis: this.millis });
    },
  },

  async mounted() {
    document.addEventListener('keyup', (event) => {
      var name = event.key;
      
      // Check if event is for the space key
      if (name === " ") {
        if (!running) {
          this.start();
        } else {
          this.stop();
        }
      }
    }, false);
  }
}
</script>

<style>
.main {
  text-align: center;
  font-size: 4.5em;
}
</style>