package eu.datlab.dataaccess.dto.parsed;

import eu.dl.dataaccess.dto.parsed.BaseParsedStorableDTO;
import eu.dl.dataaccess.dto.parsed.Parsable;
import eu.dl.dataaccess.dto.parsed.ParsedAddress;

// TODO: Auto-generated Javadoc
/**
 * Parsed Contracting Authority.
 */
public class ParsedContractingAuthority extends BaseParsedStorableDTO implements Parsable {

    private String vat;
    private String nationalRegistrationNumber;
    private String officialName;
    private ParsedAddress address;
    private String contactPoint;
    private String contactName;
    private String phone;
    private String fax;
    private String email;
    private String url;
    private String mainActivity;
    private String type;
    private String tendersWon;
    private String contractsAsCA;
    private String earnings;
    private String expenditures;
    private Boolean isSupplier;


    /**
     * Gets the vat.
     *
     * @return the vat
     */
    public final String getVat() {
        return vat;
    }


    /**
     * Sets the vat.
     *
     * @param vat
     *            the vat
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setVat(final String vat) {
        this.vat = vat;
        return this;
    }


    /**
     * Gets the national registration number.
     *
     * @return the national registration number
     */
    public final String getNationalRegistrationNumber() {
        return nationalRegistrationNumber;
    }


    /**
     * Sets the national registration number.
     *
     * @param nationalRegistrationNumber
     *            the national registration number
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setNationalRegistrationNumber(
            final String nationalRegistrationNumber) {
        this.nationalRegistrationNumber = nationalRegistrationNumber;
        return this;
    }


    /**
     * Gets the official name.
     *
     * @return the official name
     */
    public final String getOfficialName() {
        return officialName;
    }


    /**
     * Sets the official name.
     *
     * @param officialName
     *            the official name
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setOfficialName(final String officialName) {
        this.officialName = officialName;
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
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setAddress(final ParsedAddress address) {
        this.address = address;
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
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setContactPoint(final String contactPoint) {
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
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setContactName(final String contactName) {
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
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setPhone(final String phone) {
        this.phone = phone;
        return this;
    }


    /**
     * Gets the fax.
     *
     * @return the fax
     */
    public final String getFax() {
        return fax;
    }


    /**
     * Sets the fax.
     *
     * @param fax
     *            the fax
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setFax(final String fax) {
        this.fax = fax;
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
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setEmail(final String email) {
        this.email = email;
        return this;
    }


    /**
     * Gets the url.
     *
     * @return the url
     */
    public final String getUrl() {
        return url;
    }


    /**
     * Sets the url.
     *
     * @param url
     *            the url
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setUrl(final String url) {
        this.url = url;
        return this;
    }


    /**
     * Gets the main activity.
     *
     * @return the main activity
     */
    public final String getMainActivity() {
        return mainActivity;
    }


    /**
     * Sets the main activity.
     *
     * @param mainActivity
     *            the main activity
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setMainActivity(final String mainActivity) {
        this.mainActivity = mainActivity;
        return this;
    }


    /**
     * Gets the type.
     *
     * @return the type
     */
    public final String getType() {
        return type;
    }


    /**
     * Sets the type.
     *
     * @param type
     *            the type
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setType(final String type) {
        this.type = type;
        return this;
    }


    /**
     * Gets the tenders won.
     *
     * @return the tenders won
     */
    public final String getTendersWon() {
        return tendersWon;
    }


    /**
     * Sets the tenders won.
     *
     * @param tendersWon
     *            the tenders won
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setTendersWon(final String tendersWon) {
        this.tendersWon = tendersWon;
        return this;
    }


    /**
     * Gets the contracts as CA.
     *
     * @return the contracts as CA
     */
    public final String getContractsAsCA() {
        return contractsAsCA;
    }


    /**
     * Sets the contracts as CA.
     *
     * @param contractsAsCA
     *            the contracts as CA
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setContractsAsCA(final String contractsAsCA) {
        this.contractsAsCA = contractsAsCA;
        return this;
    }


    /**
     * Gets the earnings.
     *
     * @return the earnings
     */
    public final String getEarnings() {
        return earnings;
    }


    /**
     * Sets the earnings.
     *
     * @param earnings
     *            the earnings
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setEarnings(final String earnings) {
        this.earnings = earnings;
        return this;
    }


    /**
     * Gets the expenditures.
     *
     * @return the expenditures
     */
    public final String getExpenditures() {
        return expenditures;
    }


    /**
     * Sets the expenditures.
     *
     * @param expenditures
     *            the expenditures
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setExpenditures(final String expenditures) {
        this.expenditures = expenditures;
        return this;
    }


    /**
     * Gets the checks if is supplier.
     *
     * @return the checks if is supplier
     */
    public final Boolean getIsSupplier() {
        return isSupplier;
    }


    /**
     * Sets the is supplier.
     *
     * @param isSupplier
     *            the is supplier
     * @return the parsed contracting authority
     */
    public final ParsedContractingAuthority setIsSupplier(final Boolean isSupplier) {
        this.isSupplier = isSupplier;
        return this;
    }
}
