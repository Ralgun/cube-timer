<template>
  <div>
    <p>
      <span id="secs">{{ disp_secs }}</span> <span id="millis">{{ disp_millis }}</span>
    </p>
    <button @click="start">Start</button>
    <button @click="stop">Stop</button>
  </div>
</template>

<script>
import Vue from 'vue'

let timer;

export default {
  data: () => ({
    millis: 0,
    disp_secs: 0,
    disp_millis: 0,
  }),

  methods: {
    start: function() {
      clearInterval(timer);
      console.log("Start");
      
      this.millis = 0;

      timer = setInterval(() => {
        this.millis += 10;
        this.disp_secs = Math.floor(this.millis/1000);
        this.disp_millis = this.millis % 1000;
      }, 10);
    },
    stop: function() {
      clearInterval(timer);
      
      const formData= new FormData();
      formData.set("millis", this.millis);
      this.$axios.$post("/api/send-time", { tMillis: this.millis });
    },
  },

  async mounted() {
  }
}
</script>