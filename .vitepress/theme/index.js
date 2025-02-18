import DefaultTheme from 'vitepress/theme-without-fonts'
import SolutionHider from '../components/SolutionHider.vue'
import './custom.css'

export default {
    ...DefaultTheme,
    enhanceApp({ app }) {
        app.component('SolutionHider', SolutionHider)
      }
}