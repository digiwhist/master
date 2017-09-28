package eu.dl.dataaccess.dto.parsed;

import com.fasterxml.jackson.annotation.JsonTypeInfo;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import java.util.ArrayList;
import java.util.List;

// TODO: Auto-generated Javadoc
/**
 * Body (company, organization, ...).
 */
@JsonTypeInfo(use=JsonTypeInfo.Id.CLASS, include=JsonTypeInfo.As.PROPERTY, property="@class",
    defaultImpl = ParsedBody.class)
public class ParsedBody {

    /**
     * List of different body identification numbers (VAT or other unique
     * country ID).
     */
    private List<BodyIdentifier> bodyIds;

    /**
     * Official body name.
     */
    private String name;

    /**
     * Address of the body seat.
     */
    private ParsedAddress address;

    /**
     * E-mail address.
     */
    private String email;

    /**
     * Description of contact point.
     */
    private String contactPoint;

    /**
     * Name of the contact person.
     */
    private String contactName;

    /**
     * Phone number of the contact.
     */
    private String phone;

    /**
     * Identifies the leader of consortium (typically buyer or bidder).
     */
    private String isLeader;

    /**
     * Lis of main activities of body (eg. national authority, regional authority, public
     * body, etc.)
     */
    private List<String> mainActivities;

    /**
     * Type of buyer.
     */
    private String buyerType;

    /**
     * Public buyer.
     */
    private String isPublic;

    /**
     * Subsidized buyer.
     */
    private String isSubsidized;

    /**
     * Sectoral buyer - has different TED form.
     */
    private String isSectoral;

    /**
     * Body is a small-medium enterprise (SME).
     */
    private String isSme;


    /**
     * Gets the body ids.
     *
     * @return the body ids
     */
    public final List<BodyIdentifier> getBodyIds() {
        return bodyIds;
    }


    /**
     * Sets the body ids.
     *
     * @param bodyIds
     *            the body ids
     * @return the parsed body
     */
    public final ParsedBody setBodyIds(final List<BodyIdentifier> bodyIds) {
        this.bodyIds = bodyIds;
        return this;
    }


    /**
     * Gets the name.
     *
     * @return the name
     */
    public final String getName() {
        return name;
    }


    /**
     * Sets the name.
     *
     * @param name
     *            the name
     * @return the parsed body
     */
    public final ParsedBody setName(final String name) {
        this.name = name;
        return this;
    }


    /**
     * Gets the address.
     *
     * @return the address
     */
    public final ParsedAddress getAddress() {
        return address;
    }


    /**
     * Sets the address.
     *
     * @param address
     *            the address
     * @return the parsed body
     */
    public final ParsedBody setAddress(final ParsedAddress address) {
        this.address = address;
        return this;
    }


    /**
     * Gets the email.
     *
     * @return the email
     */
    public final String getEmail() {
        return email;
    }


    /**
     * Sets the email.
     *
     * @param email
     *            the email
     * @return the parsed body
     */
    public final ParsedBody setEmail(final String email) {
        this.email = email;
        return this;
    }


    /**
     * Gets the contact point.
     *
     * @return the contact point
     */
    public final String getContactPoint() {
        return contactPoint;
    }


    /**
     * Sets the contact point.
     *
     * @param contactPoint
     *            the contact point
     * @return the parsed body
     */
    public final ParsedBody setContactPoint(final String contactPoint) {
        this.contactPoint = contactPoint;
        return this;
    }


    /**
     * Gets the contact name.
     *
     * @return the contact name
     */
    public final String getContactName() {
        return contactName;
    }


    /**
     * Sets the contact name.
     *
     * @param contactName
     *            the contact name
     * @return the parsed body
     */
    public final ParsedBody setContactName(final String contactName) {
        this.contactName = contactName;
        return this;
    }


    /**
     * Gets the phone.
     *
     * @return the phone
     */
    public final String getPhone() {
        return phone;
    }


    /**
     * Sets the phone.
     *
     * @param phone
     *            the phone
     * @return the parsed body
     */
    public final ParsedBody setPhone(final String phone) {
        this.phone = phone;
        return this;
    }


    /**
     * Gets the checks if is leader.
     *
     * @return the checks if is leader
     */
    public final String getIsLeader() {
        return isLeader;
    }


    /**
     * Sets the is leader.
     *
     * @param isLeader
     *            the is leader
     * @return the parsed body
     */
    public final ParsedBody setIsLeader(final String isLeader) {
        this.isLeader = isLeader;
        return this;
    }


    /**
     * Gets the main activities.
     *
     * @return the main activities
     */
    public final List<String> getMainActivities() {
        return mainActivities;
    }


    /**
     * Sets the main activities.
     *
     * @param mainActivities
     *            the main activities
     * @return the parsed body
     */
    public final ParsedBody setMainActivities(final List<String> mainActivities) {
        this.mainActivities = mainActivities;
        return this;
    }


    /**
     * Adds the main activity.
     *
     * @param mainActivity
     *            the main activity
     * @return the parsed body
     */
    public final ParsedBody addMainActivity(final String mainActivity) {
        if (mainActivity != null && !mainActivity.trim().isEmpty()) {
            if (getMainActivities() == null) {
                setMainActivities(new ArrayList<>());
            }

            this.mainActivities.add(mainActivity);
        }

        return this;
    }


    /**
     * Gets the buyer type.
     *
     * @return the buyer type
     */
    public final String getBuyerType() {
        return buyerType;
    }


    /**
     * Sets the buyer type.
     *
     * @param buyerType
     *            the buyer type
     * @return the parsed body
     */
    public final ParsedBody setBuyerType(final String buyerType) {
        this.buyerType = buyerType;
        return this;
    }


    /**
     * Gets the checks if is public.
     *
     * @return the checks if is public
     */
    public final String getIsPublic() {
        return isPublic;
    }


    /**
     * Sets the is public.
     *
     * @param isPublic
     *            the is public
     * @return the parsed body
     */
    public final ParsedBody setIsPublic(final String isPublic) {
        this.isPublic = isPublic;
        return this;
    }


    /**
     * Gets the checks if is subsidized.
     *
     * @return the checks if is subsidized
     */
    public final String getIsSubsidized() {
        return isSubsidized;
    }


    /**
     * Sets the is subsidized.
     *
     * @param isSubsidized
     *            the is subsidized
     * @return the parsed body
     */
    public final ParsedBody setIsSubsidized(final String isSubsidized) {
        this.isSubsidized = isSubsidized;
        return this;
    }


    /**
     * Gets the checks if is sectoral.
     *
     * @return the checks if is sectoral
     */
    public final String getIsSectoral() {
        return isSectoral;
    }


    /**
     * Sets the is sectoral.
     *
     * @param isSectoral
     *            the is sectoral
     * @return the parsed body
     */
    public final ParsedBody setIsSectoral(final String isSectoral) {
        this.isSectoral = isSectoral;
        return this;
    }


    /**
     * Gets the checks if is sme.
     *
     * @return the checks if is sme
     */
    public final String getIsSme() {
        return isSme;
    }


    /**
     * Sets the is sme.
     *
     * @param isSme
     *            the is sme
     * @return the parsed body
     */
    public final ParsedBody setIsSme(final String isSme) {
        this.isSme = isSme;
        return this;
    }


    /**
     * Adds the body id.
     *
     * @param bodyId
     *            the body id
     * @return the parsed body
     */
    public final ParsedBody addBodyId(final BodyIdentifier bodyId) {
        if (bodyId != null) {
            if (this.bodyIds == null) {
                this.bodyIds = new ArrayList<>();
            }

            this.bodyIds.add(bodyId);
        }
        
        return this;
    }
}
