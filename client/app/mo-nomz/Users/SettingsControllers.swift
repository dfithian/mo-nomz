//
//  SettingsControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 5/9/21.
//

import StoreKit
import UIKit

class SettingsController: UIViewController {
    @IBOutlet weak var banner: UIView!
    
    var bannerVc: BannerController? = nil
    var profileVc: SettingsTableController? = nil
    
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
        if let vc = segue.destination as? SettingsTableController, segue.identifier == "embedProfile" {
            profileVc = vc
            vc.onChange = reloadInputViews
        }
    }
}

class SettingsTableController: UITableViewController {
    var preferences: [PreferenceRole] = [.mealsDefaultTab]
    var products: [(SKProduct, Bool)] = []
    var onChange: (() -> Void)? = nil

    let SUPPORT = 0
    let PREFERENCES = 1
    let PURCHASES = 2
    
    private func loadData() {
        let spinner = startLoading()
        Purchases.shared.getProducts(withHandler: {
            self.stopLoading(spinner)
            switch $0 {
            case .failure(let err):
                print(err.errorDescription ?? "Failed to get products")
                break
            case .success(let ps):
                self.products = []
                for p in ps {
                    if let role = ProductRole.fromString(p.productIdentifier), !role.isConsumable, User.purchased(role) {
                        self.products.append((p, true))
                    } else {
                        self.products.append((p, false))
                    }
                }
                self.products.sort(by: { $0.0.productIdentifier < $1.0.productIdentifier })
                DispatchQueue.main.async {
                    self.tableView.reloadData()
                } 
                break
            }
        })
    }
    
    @objc func didTapPreference(_ sender: Any?) {
        guard let s = sender as? UISwitch else { return }
        User.setPreference(preferences[s.tag], s.isOn)
    }
    
    override func numberOfSections(in tableView: UITableView) -> Int {
        return 3
    }
    
    override func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        switch section {
        case SUPPORT: return 2
        case PREFERENCES: return preferences.count
        case PURCHASES: return products.count + 1
        default: return 0
        }
    }
    
    override func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        switch section {
        case SUPPORT: return "Support"
        case PREFERENCES: return "Preferences"
        case PURCHASES: return "Purchases"
        default: return nil
        }
    }
    
    override func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        switch indexPath.section {
        case SUPPORT:
            if (indexPath.row == 0) {
                let cell = tableView.dequeueReusableCell(withIdentifier: "version") as! OneLabel
                let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as! String
                let bundle = Bundle.main.infoDictionary?["CFBundleVersion"] as! String
                cell.label.text = "\(version).\(bundle)"
                return cell
            } else {
                return tableView.dequeueReusableCell(withIdentifier: "help")!
            }
        case PREFERENCES:
            let cell = tableView.dequeueReusableCell(withIdentifier: "preference") as! LabelSwitch
            let preference = preferences[indexPath.row]
            cell.label.text = preference.description
            cell.switch_.tag = indexPath.row
            cell.switch_.isOn = User.preference(preference)
            cell.switch_.addTarget(self, action: #selector(didTapPreference), for: .touchUpInside)
            return cell
        case PURCHASES:
            if (products.count > indexPath.row) {
                let (product, bought) = products[indexPath.row]
                let price = Purchases.shared.getPriceFormatted(for: product) ?? ""
                let cell = tableView.dequeueReusableCell(withIdentifier: "purchase") as! LabelButton
                cell.label.text = "\(product.localizedTitle) - \(price)"
                cell.label.isEnabled = !bought
                let image = UIImage(systemName: bought ? "lock.open.fill" : "lock.fill")
                cell.button.setImage(image, for: .normal)
                return cell
            } else {
                return tableView.dequeueReusableCell(withIdentifier: "restore")!
            }
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
        case SUPPORT:
            if (indexPath.row == 1) { needHelp() }
            break
        case PURCHASES:
            if (products.count > indexPath.row) {
                let (product, bought) = products[indexPath.row]
                guard !bought else { return }
                guard let price = Purchases.shared.getPriceFormatted(for: product) else { return }
                guard Purchases.shared.canMakePayments() else { return }
                let handler = { (action: UIAlertAction) -> Void in
                    Purchases.shared.buy(product: product, withHandler: self.purchaseHandler)
                }
                buyPrompt(title: product.localizedTitle, message: product.localizedDescription, price: price, handler: handler)
            } else {
                guard Purchases.shared.canMakePayments() else { return }
                Purchases.shared.restore(withHandler: self.purchaseHandler)
            }
            break
        default: break
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        loadData()
    }
}
