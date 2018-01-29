package eu.dl.dataaccess.dto.ocds;


import eu.dl.dataaccess.annotation.Transformable;
import java.util.ArrayList;
import java.util.List;

/**
 * OCDS implementation. This object doesn't cover full OCDS schema.
 * 
 * @see <a href="http://standard.open-contracting.org/1.1/en/schema/release/">OCDS Release Schema</a>
 */
@Transformable
public class OCDSImplementation extends BaseOCDSDocumentsReferrer<OCDSImplementation> {

    private List<OCDSTransaction> transactions;

    /**
     * @return list of transactions
     */
    public final List<OCDSTransaction> getTransactions() {
        return transactions;
    }

    /**
     * @param transactions
     *      list of transactions to be set
     * @return this instance for chaining
     */
    public final OCDSImplementation setTransactions(final List<OCDSTransaction> transactions) {
        this.transactions = transactions;
        return this;
    }

    /**
     * Adds transaction. List is created if needed.
     *
     * @param transaction
     *      transaction to be added
     * @return this instance for chaining
     */
    public final OCDSImplementation addTransaction(final OCDSTransaction transaction) {
        if (transaction != null) {
            if (this.transactions == null) {
                this.transactions = new ArrayList<>();
            }

            this.transactions.add(transaction);
        }

        return this;
    }
}
