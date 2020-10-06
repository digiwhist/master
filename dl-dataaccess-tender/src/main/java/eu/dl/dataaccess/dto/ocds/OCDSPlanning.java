package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;

/**
 * OCDS planning. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSPlanning extends BaseOCDSDocumentsReferrer<OCDSPlanning> {

    private OCDSBudget budget;

    private String rationale;

    /**
     * @return budget
     */
    public final OCDSBudget getBudget() {
        return budget;
    }

    /**
     * @param budget
     *      budget to be set
     * @return this instance for chaining
     */
    public final OCDSPlanning setBudget(final OCDSBudget budget) {
        this.budget = budget;
        return this;
    }

    /**
     * @return rationale
     */
    public final String getRationale() {
        return rationale;
    }

    /**
     * @param rationale
     *      rationale to be set
     * @return this instance for chaining
     */
    public final OCDSPlanning setRationale(final String rationale) {
        this.rationale = rationale;
        return this;
    }
}
