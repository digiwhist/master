package eu.dl.dataaccess.dto.clean;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.BodyIdentifier;
import eu.dl.dataaccess.dto.codetables.BuyerActivityType;
import eu.dl.dataaccess.dto.codetables.BuyerType;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.utils.ClassUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Body (company, organization, ...).
 */
@JsonTypeInfo(use=JsonTypeInfo.Id.CLASS, include=JsonTypeInfo.As.PROPERTY, property="@class",
    defaultImpl=CleanBody.class)
@Transformable
public class CleanBody extends CleanStorableDTO implements Validable {

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
    private Address address;

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
    private Boolean isLeader;

    /**
     * List of main activity of body (eg. national authority, regional
     * authority, public body, etc.)
     */
    private List<BuyerActivityType> mainActivities;

    /**
     * Type of buyer.
     */
    private BuyerType buyerType;

    /**
     * Public buyer.
     */
    private Boolean isPublic;

    /**
     * Subsidized buyer.
     */
    private Boolean isSubsidized;

    /**
     * Sectoral buyer - has different TED form.
     */
    private Boolean isSectoral;

    /**
     * Body is a small-medium enterprise (SME).
     */
    private Boolean isSme;

    /**
     * @return the bodyIds
     */
    public final List<BodyIdentifier> getBodyIds() {
        return bodyIds;
    }

    /**
     * @param newBodyIds
     *            the bodyIds to set
     * @return this instance for chaining
     */
    public final CleanBody setBodyIds(final List<BodyIdentifier> newBodyIds) {
        this.bodyIds = newBodyIds;
        return this;
    }

    /**
     * Adds an body identifier to the bodyIds list.
     *
     * @param bodyId
     *            body identifier
     * @return this instance for chaining
     */
    public final CleanBody addBodyId(final BodyIdentifier bodyId) {
        if (bodyId != null) {
            if (this.bodyIds == null) {
                this.bodyIds = new ArrayList<>();
            }

            this.bodyIds.add(bodyId);
        }

        return this;
    }

    /**
     * @return the name
     */
    public final String getName() {
        return name;
    }

    /**
     * @param newName
     *            the name to set
     * @return this instance for chaining
     */
    public final CleanBody setName(final String newName) {
        this.name = newName;
        return this;
    }

    /**
     * @return the address
     */
    public final Address getAddress() {
        return address;
    }

    /**
     * @param newAddress
     *            the address to set
     * @return this instance for chaining
     */
    public final CleanBody setAddress(final Address newAddress) {
        this.address = newAddress;
        return this;
    }

    /**
     * @return the email
     */
    public final String getEmail() {
        return email;
    }

    /**
     * @param newEmail
     *            the email to set
     * @return this instance for chaining
     */
    public final CleanBody setEmail(final String newEmail) {
        this.email = newEmail;
        return this;
    }

    /**
     * @return the contactPoint
     */
    public final String getContactPoint() {
        return contactPoint;
    }

    /**
     * @param newContactPoint
     *            the contactPoint to set
     * @return this instance for chaining
     */
    public final CleanBody setContactPoint(final String newContactPoint) {
        this.contactPoint = newContactPoint;
        return this;
    }

    /**
     * @return the contactName
     */
    public final String getContactName() {
        return contactName;
    }

    /**
     * @param newContactName
     *            the contactName to set
     * @return this instance for chaining
     */
    public final CleanBody setContactName(final String newContactName) {
        this.contactName = newContactName;
        return this;
    }

    /**
     * @return the phone
     */
    public final String getPhone() {
        return phone;
    }

    /**
     * @param newPhone
     *            the phone to set
     * @return this instance for chaining
     */
    public final CleanBody setPhone(final String newPhone) {
        this.phone = newPhone;
        return this;
    }

    /**
     * @return the isLeader
     */
    public final Boolean getIsLeader() {
        return isLeader;
    }

    /**
     * @param newIsLeader
     *            the isLeader to set
     * @return this instance for chaining
     */
    public final CleanBody setIsLeader(final Boolean newIsLeader) {
        this.isLeader = newIsLeader;
        return this;
    }

    /**
     * @return the mainActivities
     */
    public final List<BuyerActivityType> getMainActivities() {
        return mainActivities;
    }

    /**
     * @param newMainActivities
     *            the mainActivities list to set
     * @return this instance for chaining
     */
    public final CleanBody setMainActivities(final List<BuyerActivityType> newMainActivities) {
        this.mainActivities = newMainActivities;
        return this;
    }

    /**
     * @param mainActivity
     *            the mainActivities list to set
     * @return this instance for chaining
     */
    public final CleanBody addMainActivity(final BuyerActivityType mainActivity) {
        if (mainActivity != null) {
            if (getMainActivities() == null) {
                setMainActivities(new ArrayList<>());
            }

            this.mainActivities.add(mainActivity);
        }

        return this;
    }

    /**
     * @return the buyerType
     */
    public final BuyerType getBuyerType() {
        return buyerType;
    }

    /**
     * @param newBuyerType
     *            the buyerType to set
     * @return this instance for chaining
     */
    public final CleanBody setBuyerType(final BuyerType newBuyerType) {
        this.buyerType = newBuyerType;
        return this;
    }

    /**
     * @return the isPublic
     */
    public final Boolean getIsPublic() {
        return isPublic;
    }

    /**
     * @param newIsPublic
     *            the isPublic to set
     * @return this instance for chaining
     */
    public final CleanBody setIsPublic(final Boolean newIsPublic) {
        this.isPublic = newIsPublic;
        return this;
    }

    /**
     * @return the isSubsidized
     */
    public final Boolean getIsSubsidized() {
        return isSubsidized;
    }

    /**
     * @param newIsSubsidized
     *            the isSubsidized to set
     * @return this instance for chaining
     */
    public final CleanBody setIsSubsidized(final Boolean newIsSubsidized) {
        this.isSubsidized = newIsSubsidized;
        return this;
    }

    /**
     * @return the isSectoral
     */
    public final Boolean getIsSectoral() {
        return isSectoral;
    }

    /**
     * @param newIsSectoral
     *            the isSectoral to set
     * @return this instance for chaining
     */
    public final CleanBody setIsSectoral(final Boolean newIsSectoral) {
        this.isSectoral = newIsSectoral;
        return this;
    }

    /**
     * @return the isSme
     */
    public final Boolean getIsSme() {
        return isSme;
    }

    /**
     * @param newIsSme
     *            the isSme to set
     * @return this instance for chaining
     */
    public final CleanBody setIsSme(final Boolean newIsSme) {
        this.isSme = newIsSme;
        return this;
    }

    @Override
    @JsonIgnore
    public final CleanBody getValid() {
        setAddress(ClassUtils.removeNonsenses(address));
        setBodyIds(ValidationUtils.getValid(bodyIds));
        setMainActivities(ValidationUtils.getValid(mainActivities));
        
        return ValidationUtils.getValid(this, address, bodyIds, buyerType, contactName, contactPoint, email, isLeader,
            isPublic, isSectoral, isSme, isSubsidized, mainActivities, name, phone);
    }
}
