/**
 * 
 */
package com.urs.sys.inventory;

import com.urs.db.Product;
import com.urs.sys.payment.PaymentInfo;

/**
 * @author Andrew
 */
public interface InventoryController
{
    public void addProduct(Product p);

    public void removeProduct(long id);

    public void updateProduct(Product p);

    public void sellProduct(Product p, PaymentInfo info);

    public void rentProduct(Product p, PaymentInfo info);

    public void returnProduct(Product p);
}
