<template>
  <svg class="w-full h-full" ref="svgRef" />
</template>

<script>
 import { Transformer } from 'markmap-lib';
 import { Markmap } from 'markmap-view/dist/index.esm';

 const transformer = new Transformer();
 const initValue = "";

 export default {
   name: 'App',
   data() {
     return {
       value: initValue,
     };
   },
   watch: {
     value: 'update',
   },
   methods: {
     update() {
       const { root } = transformer.transform(this.value);
       this.mm.setData(root);
       this.mm.fit();
     },

     syncContent(content) {
       var fileContent = decodeURIComponent(escape(window.atob(content)));

       if (this.value != fileContent) {
         this.value = fileContent;
       }
     }
   },
   mounted() {
     window.syncContent = this.syncContent;

     this.mm = Markmap.create(this.$refs.svgRef);
     this.update();
   },
 };
</script>
