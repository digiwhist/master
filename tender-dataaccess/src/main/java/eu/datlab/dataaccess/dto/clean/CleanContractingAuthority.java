package eu.datlab.dataaccess.dto.clean;

import com.fasterxml.jackson.annotation.JsonIgnore;
import eu.dl.dataaccess.dto.clean.CleanStorableDTO;
import eu.dl.dataaccess.dto.clean.Cleanable;
import eu.dl.dataaccess.dto.generic.Address;
import eu.dl.dataaccess.utils.ClassUtils;
import eu.dl.dataaccess.utils.ValidationUtils;

import java.net.URL;

/**
 * Clean contracting authority DTO.
 */
class CleanContractingAuthority extends CleanStorableDTO implements Cleanable {
    private String vat;
    private String nationalRegistrationNumber;
    private String officialName;
    private Address address;
    private String contactPoint;
    private String phone;
    private String fax;
    private String email;
    private URL url;
    private String mainActivity;
    private String type;
    private String tendersWon;
    private String contractsAsCA;
    private String earnings;
    private String expenditures;
    private Boolean isSupplier;

    /**
     * @return the vat
     */
    public final String getVat() {
        return vat;
    }

    /**
     * @param vat
     *            the vat to set
     */
    public final void setVat(final String vat) {
        this.vat = vat;
    }

    /**
     * @return the nationalRegistrationNumber
     */
    public final String getNationalRegistrationNumber() {
        return nationalRegistrationNumber;
    }

    /**
     * @param nationalRegistrationNumber
     *            the nationalRegistrationNumber to set
     */
    public final void setNationalRegistrationNumber(final String nationalRegistrationNumber) {
        this.nationalRegistrationNumber = nationalRegistrationNumber;
    }

    /**
     * @return the officialName
     */
    public final String getOfficialName() {
        return officialName;
    }

    /**
     * @param officialName
     *            the officialName to set
     */
    public final void setOfficialName(final String officialName) {
        this.officialName = officialName;
    }

    /**
     * @return the address
     */
    public final Address getAddress() {
        return address;
    }

    /**
     * @param address
     *            the address to set
     */
    public final void setAddress(final Address address) {
        this.address = address;
    }

    /**
     * @return the contactPoint
     */
    public final String getContactPoint() {
        return contactPoint;
    }

    /**
     * @param contactPoint
     *            the contactPoint to set
     */
    public final void setContactPoint(final String contactPoint) {
        this.contactPoint = contactPoint;
    }

    /**
     * @return the phone
     */
    public final String getPhone() {
        return phone;
    }

    /**
     * @param phone
     *            the phone to set
     */
    public final void setPhone(final String phone) {
        this.phone = phone;
    }

    /**
     * @return the fax
     */
    public final String getFax() {
        return fax;
    }

    /**
     * @param fax
     *            the fax to set
     */
    public final void setFax(final String fax) {
        this.fax = fax;
    }

    /**
     * @return the email
     */
    public final String getEmail() {
        return email;
    }

    /**
     * @param email
     *            the email to set
     */
    public final void setEmail(final String email) {
        this.email = email;
    }

    /**
     * @return the url
     */
    public final URL getUrl() {
        return url;
    }

    /**
     * @param url
     *            the url to set
     */
    public final void setUrl(final URL url) {
        this.url = url;
    }

    /**
     * @return the mainActivity
     */
    public final String getMainActivity() {
        return mainActivity;
    }

    /**
     * @param mainActivity
     *            the mainActivity to set
     */
    public final void setMainActivity(final String mainActivity) {
        this.mainActivity = mainActivity;
    }

    /**
     * @return the type
     */
    public final String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public final void setType(final String type) {
        this.type = type;
    }

    /**
     * @return the contractsWon
     */
    public final String getTendersWon() {
        return tendersWon;
    }

    /**
     * @param tendersWon
     *            the tendersWon to set
     */
    public final void setTendersWon(final String tendersWon) {
        this.tendersWon = tendersWon;
    }

    /**
     * @return the contractsAsCA
     */
    public final String getContractsAsCA() {
        return contractsAsCA;
    }

    /**
     * @param contractsAsCA
     *            the contractsAsCA to set
     */
    public final void setContractsAsCA(final String contractsAsCA) {
        this.contractsAsCA = contractsAsCA;
    }

    /**
     * @return the earnings
     */
    public final String getEarnings() {
        return earnings;
    }

    /**
     * @param earnings
     *            the earnings to set
     */
    public final void setEarnings(final String earnings) {
        this.earnings = earnings;
    }

    /**
     * @return the expenditures
     */
    public final String getExpenditures() {
        return expenditures;
    }

    /**
     * @param expenditures
     *            the expenditures to set
     */
    public final void setExpenditures(final String expenditures) {
        this.expenditures = expenditures;
    }

    /**
     * @return the isSupplier
     */
    public final Boolean getIsSupplier() {
        return isSupplier;
    }

    /**
     * @param isSupplier
     *            the isSupplier to set
     */
    public final void setIsSupplier(final Boolean isSupplier) {
        this.isSupplier = isSupplier;
    }

    @Override
    @JsonIgnore
    public final CleanContractingAuthority getValid() {
        setAddress(ClassUtils.removeNonsenses(address));

        return ValidationUtils.getValid(this, address, contactPoint, contractsAsCA, earnings, email, expenditures, fax,
            isSupplier, mainActivity, nationalRegistrationNumber, officialName, phone, tendersWon, type, url, vat);
    }
}
