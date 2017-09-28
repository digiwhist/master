package eu.dl.worker.master.plugin.specific;

import eu.dl.core.UnrecoverableException;
import eu.dl.dataaccess.dto.generic.Funding;
import eu.dl.dataaccess.dto.matched.MasterablePart;
import eu.dl.worker.master.plugin.specific.comparator.MoreValuesComparator;
import eu.dl.worker.master.plugin.MasterPlugin;
import eu.dl.worker.utils.BasePlugin;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Mastering plugin for funding.
 *
 * @param <T>
 *         matched items type
 * @param <V>
 *         item type to be mastered
 * @param <U>
 *         context items type
 */
public class FundingsPlugin<T extends MasterablePart, V, U> extends BasePlugin implements MasterPlugin<T, V, U> {
    @Override
    public final V master(final List<T> items, final V finalItem, final List<U> context) {
        try {
            final Method getter = items.get(0).getClass().getMethod("getFundings");
            final Method setter = finalItem.getClass().getMethod("setFundings", java.util.List.class);

            // put all fundings in one list
            final List<Funding> allFundings = new ArrayList<>();
            for (T tender : items) {
                List<Funding> fundings = (List<Funding>) getter.invoke(tender);
                if (fundings != null) {
                    allFundings.addAll(fundings);
                }
            }

            // match fundings by source and isEuFund
            final HashMap<String, List<Funding>> matchedFundings = new HashMap<>();
            for (Funding funding : allFundings) {
                final String hash;

                if (funding.getSource() != null) {
                    hash = funding.getSource() + funding.getIsEuFund();
                } else {
                    hash = String.valueOf(funding.getIsEuFund());
                }

                if (matchedFundings.get(hash) == null) {
                    matchedFundings.put(hash, new ArrayList<>(Arrays.asList(funding)));
                } else {
                    matchedFundings.get(hash).add(funding);
                }
            }

            // get funding with most values for each matched fundings
            List<Funding> resultFundings = new ArrayList<>();
            final Comparator<Funding> comparator = new MoreValuesComparator();

            for (Map.Entry<String, List<Funding>> fundings : matchedFundings.entrySet()) {
                Funding fundingWithMostValues = null;

                for (Funding funding : fundings.getValue().stream().sorted(comparator).collect(Collectors.toList())) {
                    fundingWithMostValues = funding;
                }
                if (fundingWithMostValues != null) {
                    resultFundings.add(fundingWithMostValues);
                }
            }

            // save null instead of an empty list
            if (resultFundings.isEmpty()) {
                resultFundings = null;
            }

            setter.invoke(finalItem, resultFundings);
        } catch (NoSuchMethodException | InvocationTargetException | IllegalAccessException e) {
            logger.error("Unable to pick the value with exception {}", e);
            throw new UnrecoverableException("Unable to pick value for exception", e);
        }

        return finalItem;
    }
}
