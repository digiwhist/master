package eu.dl.worker.indicator.plugin;

/**
 * Holds score of indicator.
 */
public class IndicatorScore {
   private int total;
   private int fails;

   /**
    * Default constructor.
    */
   IndicatorScore() {
       this.total = 0;
       this.fails = 0;
   }

   /**
    * Updates score. Increments the counter of tests and in case that {@code test} fails (false) also increments
    * counter of fails.
    *
    * @param test
    *      test.
    */
   final void test(final boolean test) {
       total++;
       if (test) {
           fails++;
       }
   }

   /**
    * @return ration as number of failed tests divided by total number of tests, or null if the score wasn't
    * initialized by calling of {@link #test(boolean)} method
    */
   final double ration() {
       return isInitialized() ? (double) fails/total * 100 : null;
   }

   /**
    * @return TRUE if the score was initialized by calling of {@link #test(boolean)} method, otherwise FALSE
    */
   final boolean isInitialized() {
       return total > 0;
   }
}