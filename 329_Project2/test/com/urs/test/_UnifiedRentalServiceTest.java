/**
 * 
 */
package com.urs.test;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

/**
 * @author Andrew
 * 
 */
@RunWith(Suite.class)
@SuiteClasses({
               CustomerTest.class,
               DatabaseTest.class,
               InventoryTest.class,
               PaymentTest.class,
               UITest.class
})
public class _UnifiedRentalServiceTest {}
