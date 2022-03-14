//
//  GroceryController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/9/21.
//

import GoogleMobileAds
import UIKit

class GroceryController: UIViewController {
    @IBOutlet weak var banner: UIView!
    @IBOutlet weak var toolbar: Toolbar!
    @IBOutlet weak var clear: UIButton!
    @IBOutlet weak var export: UIButton!
    @IBOutlet weak var add: UIButton!

    var groceryVc: GroceryListController? = nil

    @IBAction func didTapClear(_ sender: Any?) {
        if !Database.selectGroceries().isEmpty {
            let handler = { (action: UIAlertAction) -> Void in
                Database.clearAll()
                self.reloadData()
            }
            promptForConfirmation(title: "Clear", message: "Are you sure you want to clear?", handler: handler)
        }
    }

    @IBAction func didTapExport(_ sender: Any) {
        let items = Database.selectGroceries().filter({ $0.item.active })
        if !items.isEmpty {
            let exportText: String = items.map({ $0.item.render() }).joined(separator: "\n")
            let vc = UIActivityViewController(activityItems: [exportText], applicationActivities: nil)
            present(vc, animated: true, completion: nil)
        }
    }

    func reloadData() {
        groceryVc?.reloadData()
    }

    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? AddController, segue.identifier == "addGroceries" {
            vc.onChange = reloadData
        }
        if let vc = segue.destination as? GroceryListController, segue.identifier == "embedGroceryItems" {
            groceryVc = vc
        }
        if let vc = segue.destination as? BannerController, segue.identifier == "embedBanner" {
            vc.height = banner.constraints.filter({ $0.identifier == "height" }).first
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        reloadData()
        clear.frame = CGRect(x: clear.frame.minX, y: clear.frame.minY, width: clear.frame.width, height: toolbar.frame.height)
        clear.alignTextUnderImage()
        export.frame = CGRect(x: export.frame.minX, y: export.frame.minY, width: export.frame.width, height: toolbar.frame.height)
        export.alignTextUnderImage()
        add.frame = CGRect(x: add.frame.minX, y: add.frame.minY, width: add.frame.width, height: toolbar.frame.height)
        add.alignTextUnderImage()
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        reloadData()
    }
}
