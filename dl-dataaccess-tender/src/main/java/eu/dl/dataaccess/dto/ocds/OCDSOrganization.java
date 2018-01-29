package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.OCDSPartyRole;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS organization. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSOrganization extends BaseOCDSOrganization<OCDSOrganization> {

    private List<OCDSPartyRole> roles;
   
    //private Object details;

    /**
     * @return list of roles
     */
    public final List<OCDSPartyRole> getRoles() {
        return roles;
    }

    /**
     * @param roles
     *      list of roles to be set
     * @return this instance for chaining
     */
    public final OCDSOrganization setRoles(final List<OCDSPartyRole> roles) {
        this.roles = roles;
        return this;
    }

    /**
     * Adds role. List is created if needed.
     *
     * @param role
     *      role to be added
     * @return this instance for chaining
     */
    public final OCDSOrganization addRole(final OCDSPartyRole role) {
        if (role != null) {
            if (this.roles == null) {
                this.roles = new ArrayList<>();
            }

            this.roles.add(role);
        }

        return this;
    }
}
