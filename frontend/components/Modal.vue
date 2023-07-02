<template>
    <div class="modal-overlay">
      <slot @finish="test"></slot>
      <p id="close" @click="test" >X</p>
    </div>
</template>

<script>
import Vue from 'vue'

export default {
  methods: {
    test: async function () {
      this.$emit("close");
    }
  }
}
</script>

<style scoped >
@import "styles/modal_style.css";
</style>
  