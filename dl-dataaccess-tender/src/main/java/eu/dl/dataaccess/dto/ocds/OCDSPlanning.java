package eu.dl.dataaccess.dto.ocds;


import com.fasterxml.jackson.annotation.JsonProperty;
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

    private String project;

    private String projectId;

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

    /**
     * @return project name
     */
    public final String getProject() {
        return project;
    }

    /**
     * @param project
     *      project name to be set
     * @return this instance for chaining
     */
    public final OCDSPlanning setProject(final String project) {
        this.project = project;
        return this;
    }

    /**
     * @return project eternal identifier
     */
    public final String getProjectId() {
        return projectId;
    }

    /**
     * @param projectId
     *      project eternal identifier to be set
     * @return this instance for chaining
     */
    @JsonProperty("projectID")
    public final OCDSPlanning setProjectId(final String projectId) {
        this.projectId = projectId;
        return this;
    }
}
