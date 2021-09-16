//
//  Purchases.swift
//  mo-nomz
//
//  Created by Dan Fithian on 9/15/21.
//

import StoreKit

enum ProductRole {
    case removeAds
    case donate
    
    var productIdentifier: String {
        switch self {
        case .removeAds: return "com.monomz.removeads"
        case .donate: return "com.monomz.donate"
        }
    }
    
    var isConsumable: Bool {
        switch self {
        case .removeAds: return false
        case .donate: return true
        }
    }
    
    static func fromString(_ x: String) -> ProductRole? {
        switch x {
        case "com.monomz.removeads": return .removeAds
        case "com.monomz.donate": return .donate
        default: return nil
        }
    }
}

extension Purchases.PurchasesError: LocalizedError {
    var errorDescription: String? {
        switch self {
        case .noProductIdsFound: return "No In-App Purchase product identifiers were found."
        case .noProductsFound: return "No In-App Purchases were found."
        case .productRequestFailed: return "Unable to fetch available In-App Purchase products."
        case .paymentWasCancelled: return "In-App Purchase process was cancelled."
        }
    }
}

class Purchases: NSObject, SKProductsRequestDelegate, SKPaymentTransactionObserver {
    var onReceiveProductsHandler: ((Result<[SKProduct], PurchasesError>) -> Void)?
    var onBuyProductHandler: ((Result<(), Error>) -> Void)?
    static let shared = Purchases()
 
    private override init() {
        super.init()
    }
    
    enum PurchasesError: Error {
        case noProductIdsFound
        case noProductsFound
        case paymentWasCancelled
        case productRequestFailed
    }
    
    func getProductIds() -> [String]? {
        guard let url = Bundle.main.url(forResource: "Purchases", withExtension: "plist") else { return nil }
        do {
            let data = try Data(contentsOf: url)
            let productIds = try PropertyListSerialization.propertyList(from: data, options: .mutableContainersAndLeaves, format: nil) as? [String] ?? []
            return productIds
        } catch {
            print(error.localizedDescription)
            return nil
        }
    }
    
    func getProducts(withHandler productsReceiveHandler: @escaping (_ result: Result<[SKProduct], PurchasesError>) -> Void) {
        onReceiveProductsHandler = productsReceiveHandler
        guard let productIds = getProductIds() else {
            onReceiveProductsHandler?(.failure(.noProductIdsFound))
            return
        }
        let request = SKProductsRequest(productIdentifiers: Set(productIds))
        request.delegate = self
        request.start()
    }
    
    func productsRequest(_ request: SKProductsRequest, didReceive response: SKProductsResponse) {
        let products = response.products
        if products.count > 0 {
            onReceiveProductsHandler?(.success(products))
        } else {
            onReceiveProductsHandler?(.failure(.noProductsFound))
        }
    }
    
    func request(_ request: SKRequest, didFailWithError error: Error) {
        print(error)
        onReceiveProductsHandler?(.failure(.productRequestFailed))
    }
    
    func getPriceFormatted(for product: SKProduct) -> String? {
        let formatter = NumberFormatter()
        formatter.numberStyle = .currency
        formatter.locale = product.priceLocale
        return formatter.string(from: product.price)
    }
    
    func startObserving() {
        SKPaymentQueue.default().add(self)
    }
    
    func stopObserving() {
        SKPaymentQueue.default().remove(self)
    }
    
    func canMakePayments() -> Bool {
        return SKPaymentQueue.canMakePayments()
    }
    
    func buy(product: SKProduct, withHandler handler: @escaping ((_ result: Result<(), Error>) -> Void)) {
        let payment = SKPayment(product: product)
        SKPaymentQueue.default().add(payment)
        onBuyProductHandler = handler
    }
    
    func paymentQueue(_ queue: SKPaymentQueue, updatedTransactions transactions: [SKPaymentTransaction]) {
        transactions.forEach({
            switch $0.transactionState {
            case .purchased, .restored:
                onBuyProductHandler?(.success(()))
                SKPaymentQueue.default().finishTransaction($0)
                break
            case .failed:
                if let error = $0.error as? SKError {
                    if error.code != .paymentCancelled {
                        onBuyProductHandler?(.failure(error))
                    } else {
                        onBuyProductHandler?(.failure(PurchasesError.paymentWasCancelled))
                    }
                    print("Purchase error:", error.localizedDescription)
                }
                SKPaymentQueue.default().finishTransaction($0)
                break
            default:
                break
            }
        })
    }
}
