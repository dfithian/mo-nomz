//
//  ProfileController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/12/21.
//

import StoreKit
import UIKit

class ProfileController: UITableViewController {
    var boughtProducts: [SKProduct] = []
    var unboughtProducts: [SKProduct] = []
    var onChange: (() -> Void)? = nil
    
    let SUPPORT_HEADING = 0
    let THANKS = 1
    let HELP = 2
    let PURCHASE_HEADING = 3
    let PURCHASES = 4
    let AVAILABLE_PURCHASES = 5
    let RESTORE_PURCHASES = 6
    
    private func loadData() {
        let spinner = startLoading()
        print("loading")
        Purchases.shared.getProducts(withHandler: {
            self.stopLoading(spinner)
            switch $0 {
            case .failure(let err):
                print(err.errorDescription ?? "Failed to get products")
                break
            case .success(let products):
                var bought: [SKProduct] = []
                var unbought: [SKProduct] = []
                for product in products {
                    if let role = ProductRole.fromString(product.productIdentifier), !role.isConsumable, User.purchased(role) {
                        bought.append(product)
                        User.setDidPurchase(role)
                    } else {
                        unbought.append(product)
                    }
                }
                bought.sort(by: { $0.productIdentifier < $1.productIdentifier })
                unbought.sort(by: { $0.productIdentifier < $1.productIdentifier })
                self.boughtProducts = bought
                self.unboughtProducts = unbought
                DispatchQueue.main.async {
                    self.tableView.reloadData()
                }
                break
            }
        })
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 7
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case SUPPORT_HEADING: return 1
        case THANKS: return 1
        case HELP: return 1
        case PURCHASE_HEADING: return 1
        case PURCHASES: return boughtProducts.count
        case AVAILABLE_PURCHASES: return unboughtProducts.count
        case RESTORE_PURCHASES: return 1
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case SUPPORT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Support"
            return cell
        case THANKS:
            return tableView.dequeueReusableCell(withIdentifier: "thanks")!
        case HELP:
            return tableView.dequeueReusableCell(withIdentifier: "help")!
        case PURCHASE_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
            cell.label.text = "Purchases"
            return cell
        case PURCHASES:
            let product = boughtProducts[indexPath.row]
            let price = Purchases.shared.getPriceFormatted(for: product) ?? ""
            let cell = tableView.dequeueReusableCell(withIdentifier: "purchase") as! PurchaseItem
            cell.label.text = "\(product.localizedTitle) - \(price)"
            cell.label.isEnabled = false
            let image = UIImage(systemName: "lock.open.fill")
            cell.indicator.setImage(image, for: .normal)
            return cell
        case AVAILABLE_PURCHASES:
            let product = unboughtProducts[indexPath.row]
            let price = Purchases.shared.getPriceFormatted(for: product) ?? ""
            let cell = tableView.dequeueReusableCell(withIdentifier: "purchase") as! PurchaseItem
            cell.label.text = "\(product.localizedTitle) - \(price)"
            let image = UIImage(systemName: "lock.fill")
            cell.indicator.setImage(image, for: .normal)
            return cell
        case RESTORE_PURCHASES:
            return tableView.dequeueReusableCell(withIdentifier: "restore")!
        default:
            return tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! SimpleSectionHeader
        }
    }
    
    private func purchaseHandler(_ result: Result<(), Error>) {
        switch result {
        case .failure(let error):
            switch error as? Purchases.PurchasesError {
            case .paymentWasCancelled: return
            default:
                print(error)
                self.alertUnsuccessful("Failed to complete purchase. Please try again later.")
                return
            }
        case .success(()):
            print("success")
            self.loadData()
            self.onChange?()
        }
    }
    
    override func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        switch indexPath.section {
        case HELP:
            needHelp()
            break
        case AVAILABLE_PURCHASES:
            let product = unboughtProducts[indexPath.row]
            guard let price = Purchases.shared.getPriceFormatted(for: product) else { return }
            guard Purchases.shared.canMakePayments() else { return }
            let handler = { (action: UIAlertAction) -> Void in
                Purchases.shared.buy(product: product, withHandler: self.purchaseHandler)
            }
            buyPrompt(title: product.localizedTitle, message: product.localizedDescription, price: price, handler: handler)
            break
        case RESTORE_PURCHASES:
            guard Purchases.shared.canMakePayments() else { return }
            Purchases.shared.restore(withHandler: self.purchaseHandler)
            break
        default: break
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()
    }
}
