//
//  GroceryAddController.swift
//  mo-nomz
//
//  Created by Dan Fithian on 4/23/21.
//

import UIKit

class GroceryAddController: UIViewController {
    var onChange: (() -> Void)? = nil
    var individualVc: GroceryAddItemController? = nil
    var bulkVc: GroceryAddBlobController? = nil
    
    @IBOutlet weak var segment: UISegmentedControl!
    @IBOutlet weak var individualView: UIView!
    @IBOutlet weak var bulkView: UIView!

    @IBAction func didTapCancel(_ sender: Any?) {
        cancel()
    }
    
    @IBAction func didTapSave(_ sender: Any?) {
        switch segment.selectedSegmentIndex {
        case 0:
            individualVc?.save(onChange, onCancel: cancel)
        default:
            bulkVc?.save(onChange, onCancel: cancel)
        }
    }
    
    @IBAction func didTapSegment(_ sender: UISegmentedControl) {
        switch sender.selectedSegmentIndex {
        case 0:
            self.individualView.alpha = 1
            self.bulkView.alpha = 0
            self.individualVc?.name.becomeFirstResponder()
            break
        default:
            self.individualView.alpha = 0
            self.bulkView.alpha = 1
            self.bulkVc?.blob.becomeFirstResponder()
            break
        }
    }
    
    func cancel() {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? GroceryAddItemController, segue.identifier == "embedIndividual" {
            individualVc = vc
        }
        if let vc = segue.destination as? GroceryAddBlobController, segue.identifier == "embedBulk" {
            bulkVc = vc
        }
    }
    
    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        individualView.alpha = 1
        bulkView.alpha = 0
    }
}
