package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import eu.dl.dataaccess.dto.codetables.OCDSAwardCriteria;
import eu.dl.dataaccess.dto.codetables.OCDSProcedureMethod;
import eu.dl.dataaccess.dto.codetables.OCDSProcurementCategory;
import eu.dl.dataaccess.dto.codetables.OCDSSubmissionMethod;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS tender. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSTender extends BaseOCDSDocumentsReferrer<OCDSTender> {

    private String id;

    private String title;

    private String description;

    private OCDSValue value;

    private OCDSValue minValue;

    /**
     * tender/procedureType mapped to OCDS codelist.
     */
    private OCDSProcedureMethod procurementMethod;
    
    private String procurementMethodDetails;

    private String procurementMethodRationale;

    /**
     * tender/supplyType mapped to OCDS codelist.
     */
    private OCDSProcurementCategory mainProcurementCategory;

    /**
     * tender/selectionMethod mapped to OCDS codelist.
     */
    private OCDSAwardCriteria awardCriteria;
    
    private String awardCriteriaDetails;

    /**
     * if tender/isElectronicAuction then "electronicAuction" codelist value.
     */
    private List<OCDSSubmissionMethod> submissionMethod;

    private String submissionMethodDetails;
    
    private String eligibilityCriteria;

    private OCDSPeriod tenderPeriod;

    private OCDSPeriod awardPeriod;

    private OCDSPeriod contractPeriod;

    private OCDSPeriod enquiryPeriod;

    private OCDSOrganizationReference procuringEntity;

    private Integer numberOfTenderers;

    private List<OCDSItem> items;

    private List<OCDSLot> lots;

    /**
     * @return id
     */
    public final String getId() {
        return id;
    }

    /**
     * @param id
     *      id to be set
     * @return this instance for chaining
     */
    public final OCDSTender setId(final String id) {
        this.id = id;
        return this;
    }

    /**
     * @return title
     */
    public final String getTitle() {
        return title;
    }

    /**
     * @param title
     *      title to be set
     * @return this instance for chaining
     */
    public final OCDSTender setTitle(final String title) {
        this.title = title;
        return this;
    }

    /**
     * @return description
     */
    public final String getDescription() {
        return description;
    }

    /**
     * @param description
     *      description to be set
     * @return this instance for chaining
     */
    public final OCDSTender setDescription(final String description) {
        this.description = description;
        return this;
    }

    /**
     * @return value
     */
    public final OCDSValue getValue() {
        return value;
    }

    /**
     * @param value
     *      value to be set
     * @return this instance for chaining
     */
    public final OCDSTender setValue(final OCDSValue value) {
        this.value = value;
        return this;
    }

    /**
     * @return min value
     */
    public final OCDSValue getMinValue() {
        return minValue;
    }

    /**
     * @param minValue
     *      min value to be set
     * @return this instance for chaining
     */
    public final OCDSTender setMinValue(final OCDSValue minValue) {
        this.minValue = minValue;
        return this;
    }

    /**
     * @return procurement method
     */
    public final OCDSProcedureMethod getProcurementMethod() {
        return procurementMethod;
    }

    /**
     * @param procurementMethod
     *      procurement method to be set
     * @return this instance for chaining
     */
    public final OCDSTender setProcurementMethod(final OCDSProcedureMethod procurementMethod) {
        this.procurementMethod = procurementMethod;
        return this;
    }

    /**
     * @return procurement method details
     */
    public final String getProcurementMethodDetails() {
        return procurementMethodDetails;
    }

    /**
     * @param procurementMethodDetails
     *      procurement method details to be set
     * @return this instance for chaining
     */
    public final OCDSTender setProcurementMethodDetails(final String procurementMethodDetails) {
        this.procurementMethodDetails = procurementMethodDetails;
        return this;
    }

    /**
     * @return procurement method rationale
     */
    public final String getProcurementMethodRationale() {
        return procurementMethodRationale;
    }

    /**
     * @param procurementMethodRationale
     *      procurement method rationale to be set
     * @return this instance for chaining
     */
    public final OCDSTender setProcurementMethodRationale(final String procurementMethodRationale) {
        this.procurementMethodRationale = procurementMethodRationale;
        return this;
    }

    /**
     * @return main procurement category
     */
    public final OCDSProcurementCategory getMainProcurementCategory() {
        return mainProcurementCategory;
    }

    /**
     * @param mainProcurementCategory
     *      main procurement category to be set
     * @return this instance for chaining
     */
    public final OCDSTender setMainProcurementCategory(final OCDSProcurementCategory mainProcurementCategory) {
        this.mainProcurementCategory = mainProcurementCategory;
        return this;
    }

    /**
     * @return award criteria
     */
    public final OCDSAwardCriteria getAwardCriteria() {
        return awardCriteria;
    }

    /**
     * @param awardCriteria
     *      award criteria to be set
     * @return this instance for chaining
     */
    public final OCDSTender setAwardCriteria(final OCDSAwardCriteria awardCriteria) {
        this.awardCriteria = awardCriteria;
        return this;
    }

    /**
     * @return award criteria details
     */
    public final String getAwardCriteriaDetails() {
        return awardCriteriaDetails;
    }

    /**
     * @param awardCriteriaDetails
     *      award criteria details to be set
     * @return this instance for chaining
     */
    public final OCDSTender setAwardCriteriaDetails(final String awardCriteriaDetails) {
        this.awardCriteriaDetails = awardCriteriaDetails;
        return this;
    }

    /**
     * @return submission method details
     */
    public final String getSubmissionMethodDetails() {
        return submissionMethodDetails;
    }

    /**
     * @param submissionMethodDetails
     *      submission method details to be set
     * @return this instance for chaining
     */
    public final OCDSTender setSubmissionMethodDetails(final String submissionMethodDetails) {
        this.submissionMethodDetails = submissionMethodDetails;
        return this;
    }

    /**
     * @return eligibility criteria
     */
    public final String getEligibilityCriteria() {
        return eligibilityCriteria;
    }

    /**
     * @param eligibilityCriteria
     *      eligibility criteria to be set
     * @return this instance for chaining
     */
    public final OCDSTender setEligibilityCriteria(final String eligibilityCriteria) {
        this.eligibilityCriteria = eligibilityCriteria;
        return this;
    }

    /**
     * @return tender period
     */
    public final OCDSPeriod getTenderPeriod() {
        return tenderPeriod;
    }

    /**
     * @param tenderPeriod
     *      tender period to be set
     * @return this instance for chaining
     */
    public final OCDSTender setTenderPeriod(final OCDSPeriod tenderPeriod) {
        this.tenderPeriod = tenderPeriod;
        return this;
    }

    /**
     * @return award period
     */
    public final OCDSPeriod getAwardPeriod() {
        return awardPeriod;
    }

    /**
     * @param awardPeriod
     *      award period to be set
     * @return this instance for chaining
     */
    public final OCDSTender setAwardPeriod(final OCDSPeriod awardPeriod) {
        this.awardPeriod = awardPeriod;
        return this;
    }

    /**
     * @return contract period
     */
    public final OCDSPeriod getContractPeriod() {
        return contractPeriod;
    }

    /**
     * @param contractPeriod
     *      contract period to be set
     * @return this instance for chaining
     */
    public final OCDSTender setContractPeriod(final OCDSPeriod contractPeriod) {
        this.contractPeriod = contractPeriod;
        return this;
    }

    /**
     * @return enquiry period
     */
    public final OCDSPeriod getEnquiryPeriod() {
        return enquiryPeriod;
    }

    /**
     * @param enquiryPeriod
     *      enquiry period to be set
     * @return this instance for chaining
     */
    public final OCDSTender setEnquiryPeriod(final OCDSPeriod enquiryPeriod) {
        this.enquiryPeriod = enquiryPeriod;
        return this;
    }

    /**
     * @return procuring entity
     */
    public final OCDSOrganizationReference getProcuringEntity() {
        return procuringEntity;
    }

    /**
     * @param procuringEntity
     *      procuring entity to be set
     * @return this instance for chaining
     */
    public final OCDSTender setProcuringEntity(final OCDSOrganizationReference procuringEntity) {
        this.procuringEntity = procuringEntity;
        return this;
    }

    /**
     * @return number of tenderers
     */
    public final Integer getNumberOfTenderers() {
        return numberOfTenderers;
    }

    /**
     * @param numberOfTenderers
     *      number of tenderers to be set
     * @return this instance for chaining
     */
    public final OCDSTender setNumberOfTenderers(final Integer numberOfTenderers) {
        this.numberOfTenderers = numberOfTenderers;
        return this;
    }

    /**
     * @return list of items
     */
    public final List<OCDSItem> getItems() {
        return items;
    }

    /**
     * @param items
     *      list of items to be set
     * @return this instance for chaining
     */
    public final OCDSTender setItems(final List<OCDSItem> items) {
        this.items = items;
        return this;
    }

    /**
     * Adds item. List is created if needed.
     *
     * @param item
     *      item to be added
     * @return this instance for chaining
     */
    public final OCDSTender addItem(final OCDSItem item) {
        if (item != null) {
            if (this.items == null) {
                this.items = new ArrayList<>();
            }

            this.items.add(item);
        }

        return this;
    }

    /**
     * Adds items. List is created if needed.
     *
     * @param newItems
     *      list of items to be added
     * @return this instance for chaining
     */
    public final OCDSTender addItems(final List<OCDSItem> newItems) {
        if (newItems != null) {
            if (this.items == null) {
                this.items = new ArrayList<>();
            }

            this.items.addAll(newItems);
        }

        return this;
    }

    /**
     * @return lots
     */
    public final List<OCDSLot> getLots() {
        return lots;
    }

    /**
     * @param lots
     *      list of lots to be set
     * @return this instance for chaining
     */
    public final OCDSTender setLots(final List<OCDSLot> lots) {
        this.lots = lots;
        return this;
    }

    /**
     * Adds lot. List is created if needed.
     *
     * @param lot
     *      lot to be added
     * @return this instance for chaining
     */
    public final OCDSTender addLot(final OCDSLot lot) {
        if (lot != null) {
            if (this.lots == null) {
                this.lots = new ArrayList<>();
            }

            this.lots.add(lot);
        }

        return this;
    }

    /**
     * @return list of submission methods
     */
    public final List<OCDSSubmissionMethod> getSubmissionMethod() {
        return submissionMethod;
    }

    /**
     * @param submissionMethod
     *      list of submission methods to be set
     * @return this instance for chaining
     */
    public final OCDSTender setSubmissionMethod(final List<OCDSSubmissionMethod> submissionMethod) {
        this.submissionMethod = submissionMethod;
        return this;
    }

    /**
     * Adds submission method. List is created if needed.
     *
     * @param method
     *      method to be added
     * @return this instance for chaining
     */
    public final OCDSTender addSubmissionMethod(final OCDSSubmissionMethod method) {
        if (method != null) {
            if (this.submissionMethod == null) {
                this.submissionMethod = new ArrayList<>();
            }

            this.submissionMethod.add(method);
        }

        return this;
    }
}
