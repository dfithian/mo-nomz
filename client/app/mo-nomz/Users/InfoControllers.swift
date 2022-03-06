//
//  InfoControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 5/9/21.
//

import StoreKit
import UIKit

class InfoController: UIViewController {
    @IBOutlet weak var banner: UIView!
    
    var bannerVc: BannerController? = nil
    var profileVc: InfoTableController? = nil
    
    override func reloadInputViews() {
        super.reloadInputViews()
        bannerVc?.reloadInputViews()
        profileVc?.reloadInputViews()
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
            bannerVc = vc
        }
        if let vc = segue.destination as? InfoTableController, segue.identifier == "embedProfile" {
            profileVc = vc
            vc.onChange = reloadInputViews
        }
    }
}

class InfoTableController: UITableViewController {
    var boughtProducts: [SKProduct] = []
    var unboughtProducts: [SKProduct] = []
    var preferences: [PreferenceRole] = [.mealsDefaultTab, .noTips]
    var onChange: (() -> Void)? = nil
    
    let SUPPORT_HEADING = 0
    let VERSION = 1
    let HELP = 2
    let PREFERENCE_HEADING = 3
    let PREFERENCES = 4
    let PURCHASE_HEADING = 5
    let PURCHASES = 6
    let AVAILABLE_PURCHASES = 7
    let RESTORE_PURCHASES = 8
    
    private func loadData() {
        let spinner = startLoading()
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
    
    @objc func didTapPreference(_ sender: Any?) {
        guard let s = sender as? UISwitch else { return }
        User.setPreference(preferences[s.tag], value: s.isOn)
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 9
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case SUPPORT_HEADING: return 1
        case VERSION: return 1
        case HELP: return 1
        case PURCHASE_HEADING: return 1
        case PURCHASES: return boughtProducts.count
        case AVAILABLE_PURCHASES: return unboughtProducts.count
        case RESTORE_PURCHASES: return 1
        case PREFERENCE_HEADING: return 1
        case PREFERENCES: return preferences.count
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case SUPPORT_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Support"
            return cell
        case VERSION:
            let cell = tableView.dequeueReusableCell(withIdentifier: "version") as! OneLabel
            let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as! String
            let bundle = Bundle.main.infoDictionary?["CFBundleVersion"] as! String
            cell.label.text = "\(version).\(bundle)"
            return cell
        case HELP:
            return tableView.dequeueReusableCell(withIdentifier: "help")!
        case PURCHASE_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Purchases"
            return cell
        case PURCHASES:
            let product = boughtProducts[indexPath.row]
            let price = Purchases.shared.getPriceFormatted(for: product) ?? ""
            let cell = tableView.dequeueReusableCell(withIdentifier: "purchase") as! LabelButton
            cell.label.text = "\(product.localizedTitle) - \(price)"
            cell.label.isEnabled = false
            let image = UIImage(systemName: "lock.open.fill")
            cell.button.setImage(image, for: .normal)
            return cell
        case AVAILABLE_PURCHASES:
            let product = unboughtProducts[indexPath.row]
            let price = Purchases.shared.getPriceFormatted(for: product) ?? ""
            let cell = tableView.dequeueReusableCell(withIdentifier: "purchase") as! LabelButton
            cell.label.text = "\(product.localizedTitle) - \(price)"
            let image = UIImage(systemName: "lock.fill")
            cell.button.setImage(image, for: .normal)
            return cell
        case RESTORE_PURCHASES:
            return tableView.dequeueReusableCell(withIdentifier: "restore")!
        case PREFERENCE_HEADING:
            let cell = tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
            cell.label.text = "Preferences"
            return cell
        case PREFERENCES:
            let cell = tableView.dequeueReusableCell(withIdentifier: "preference") as! LabelSwitch
            let preference = preferences[indexPath.row]
            cell.label.text = preference.description
            cell.switch_.tag = indexPath.row
            cell.switch_.isOn = User.preference(preference)
            cell.switch_.addTarget(self, action: #selector(didTapPreference), for: .touchUpInside)
            return cell
        default:
            return tableView.dequeueReusableCell(withIdentifier: "sectionHeader") as! OneLabel
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
