//
//  AddPhotoControllers.swift
//  mo-nomz
//
//  Created by Dan Fithian on 1/31/22.
//

import MobileCoreServices
import UIKit
import Vision

enum ScrapeType {
    case ingredient
    case step
}

struct ScrapeInfo {
    var image: UIImage
    var value: String
}

struct Scrape {
    var ingredients: [ScrapeInfo]
    var steps: [ScrapeInfo]
}

class AddPhotoController: UIViewController {
    @IBOutlet weak var switcher: UIBarButtonItem!

    var scrape: Scrape = Scrape(ingredients: [], steps: [])
    var current: ScrapeInfo? = nil
    var navigationVc: AddController? = nil
    var pickVc: PickPhotoController? = nil
    
    @IBAction func didTapCancel(_ sender: Any?) {
        DispatchQueue.main.async {
            self.dismiss(animated: true, completion: nil)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        performSegue(withIdentifier: "pushManualRecipe", sender: nil)
    }
    
    private func setupSwitcher() {
        switcher.menu = UIMenu(options: .displayInline, children: [
            UIAction(title: "Add link", image: UIImage(systemName: "link"), state: .off, handler: { _ in self.navigationVc?.switchToLink() }),
            UIAction(title: "Add manual", image: UIImage(systemName: "pencil"), state: .off, handler: { _ in self.navigationVc?.switchToManual() }),
            UIAction(title: "Add photos", image: UIImage(systemName: "photo.on.rectangle"), state: .on, handler: { _ in self.navigationVc?.switchToPhoto() })
        ])
    }
    
    override func prepare(for segue: UIStoryboardSegue, sender: Any?) {
        if let vc = segue.destination as? PickPhotoController, segue.identifier == "embedPhotos" {
            pickVc = vc
            vc.addVc = self
        }
        if let vc = segue.destination as? AddManualController, segue.identifier == "pushManualRecipe" {
            vc.change = .photoReview
            vc.isActive = true
            vc.ingredients = scrape.ingredients.map({ $0.value }).joined(separator: "\n")
            if scrape.steps.isEmpty {
                vc.isRecipe = false
            } else {
                vc.isRecipe = true
                vc.steps = scrape.steps.map({ $0.value }).joined(separator: "\n")
            }
        }
        if let vc = segue.destination as? ReviewPhotoController, segue.identifier == "reviewScrape" {
            vc.addVc = self
            vc.scrapeType = .ingredient
            vc.scrapeInfo = current
        }
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        setupSwitcher()
    }
}

class PickPhotoController: UICollectionViewController, UICollectionViewDelegateFlowLayout, UIImagePickerControllerDelegate, UINavigationControllerDelegate {
    var addVc: AddPhotoController? = nil
    
    let INGREDIENTS = 0
    let STEPS = 1
    let ADD = 2
    
    @objc func didTapAddPhoto(_ sender: Any?) {
        let picker = UIImagePickerController()
        picker.delegate = self
        present(picker, animated: true)
    }
    
    override func numberOfSections(in collectionView: UICollectionView) -> Int {
        return 3
    }
    
    override func collectionView(_ collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        switch section {
        case INGREDIENTS: return addVc?.scrape.ingredients.count ?? 0
        case STEPS: return addVc?.scrape.steps.count ?? 0
        case ADD: return 1
        default: return 0
        }
    }
    
    override func collectionView(_ collectionView: UICollectionView, cellForItemAt indexPath: IndexPath) -> UICollectionViewCell {
        switch indexPath.section {
        case INGREDIENTS:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "imageItem", for: indexPath) as! OneImage
            cell.layer.cornerRadius = 10
            cell.image.image = addVc!.scrape.ingredients[indexPath.row].image
            return cell
        case STEPS:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "imageItem", for: indexPath) as! OneImage
            cell.layer.cornerRadius = 10
            cell.image.image = addVc!.scrape.steps[indexPath.row].image
            return cell
        default:
            let cell = collectionView.dequeueReusableCell(withReuseIdentifier: "addImage", for: indexPath) as! OneImageButton
            cell.layer.cornerRadius = 10
            cell.button.addTarget(self, action: #selector(didTapAddPhoto), for: .touchUpInside)
            return cell
        }
    }
    
    override func collectionView(_ collectionView: UICollectionView, contextMenuConfigurationForItemAt: IndexPath, point: CGPoint) -> UIContextMenuConfiguration? {
        let handler: (() -> Void)
        switch contextMenuConfigurationForItemAt.section {
        case INGREDIENTS:
            handler = { self.addVc?.scrape.ingredients.remove(at: contextMenuConfigurationForItemAt.row) }
            break
        case STEPS:
            handler = { self.addVc?.scrape.steps.remove(at: contextMenuConfigurationForItemAt.row) }
            break
        default: return nil
        }
        let actions: UIContextMenuActionProvider = { _ in
            return UIMenu(children: [
                UIAction(title: "Remove", attributes: .destructive, handler: { _ in
                    handler()
                    self.collectionView.reloadData()
                })
            ])
        }
        return UIContextMenuConfiguration(identifier: nil, previewProvider: nil, actionProvider: actions)
    }
    
    override func collectionView(_ collectionView: UICollectionView, didSelectItemAt indexPath: IndexPath) {
        switch indexPath.section {
        case INGREDIENTS:
            self.addVc?.current = addVc?.scrape.ingredients[indexPath.row]
            self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
            break
        case STEPS:
            self.addVc?.current = addVc?.scrape.steps[indexPath.row]
            self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
        case ADD:
            didTapAddPhoto(nil)
            break
        default: break
        }
    }
    
    func collectionView(_ collectionView: UICollectionView, layout: UICollectionViewLayout, sizeForItemAt: IndexPath) -> CGSize {
        let size = (view.frame.size.width / 3) - 10
        return CGSize(width: size, height: size)
    }
    
    func collectionView(_ collectionView: UICollectionView, layout: UICollectionViewLayout, insetForSectionAt: Int) -> UIEdgeInsets {
        return UIEdgeInsets(top: 5, left: 5, bottom: 5, right: 5)
    }
    
    override func collectionView(_ collectionView: UICollectionView, viewForSupplementaryElementOfKind kind: String, at indexPath: IndexPath) -> UICollectionReusableView {
        let header = collectionView.dequeueReusableSupplementaryView(ofKind: kind, withReuseIdentifier: "collectionHeader", for: indexPath) as! CollectionHeader
        switch indexPath.section {
        case INGREDIENTS:
            header.label.text = "Ingredients"
            break
        case STEPS:
            header.label.text = "Steps"
            break
        default:
            header.label.text = "Add photo"
            break
        }
        return header
    }
    
    func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey : Any]) {
        let progress = startLoading()
        picker.dismiss(animated: true, completion: nil)
        if let image = info[.originalImage] as? UIImage {
            let textHandler = { (req: VNRequest, error: Error?) in
                self.stopLoading(progress)
                guard let observations = req.results as? [VNRecognizedTextObservation] else { return }
                let recognized = observations.compactMap { $0.topCandidates(1).first?.string }
                self.addVc?.current = ScrapeInfo(image: image, value: recognized.joined(separator: "\n"))
                self.addVc?.performSegue(withIdentifier: "reviewScrape", sender: nil)
            }
            let handler = VNImageRequestHandler(cgImage: image.cgImage!)
            let req = VNRecognizeTextRequest(completionHandler: textHandler)
            do {
                try handler.perform([req])
            } catch {
                defaultOnError(error)
            }
        }
    }
}

class ReviewPhotoController: UIViewController, UIPickerViewDelegate, UIPickerViewDataSource, UITextViewDelegate {
    @IBOutlet weak var picker: UIPickerView!
    @IBOutlet weak var text: UITextView!
    
    var scrapeType: ScrapeType? = nil
    var scrapeInfo: ScrapeInfo? = nil
    var navigationVc: AddController? = nil
    var addVc: AddPhotoController? = nil
    var beforeHeight: CGFloat? = nil
    
    let INGREDIENTS = 0
    let STEPS = 1
    
    @IBAction func didTapBack(_ sender: Any?) {
        DispatchQueue.main.async {
            self.navigationVc?.popViewController(animated: true)
        }
    }
    
    @IBAction func didTapSubmit(_ sender: Any?) {
        if let info = scrapeInfo {
            switch scrapeType {
            case .ingredient:
                addVc?.scrape.ingredients.append(info)
                break
            case .step:
                addVc?.scrape.steps.append(info)
                break
            default: break
            }
        }
        DispatchQueue.main.async {
            self.navigationVc?.popViewController(animated: true)
            self.addVc?.pickVc?.collectionView.reloadData()
        }
    }
    
    func numberOfComponents(in pickerView: UIPickerView) -> Int {
        1
    }
    
    func pickerView(_ pickerView: UIPickerView, numberOfRowsInComponent component: Int) -> Int {
        2
    }
    
    func pickerView(_ pickerView: UIPickerView, titleForRow row: Int, forComponent component: Int) -> String? {
        switch row {
        case INGREDIENTS: return "Ingredients"
        case STEPS: return "Steps"
        default: return nil
        }
    }
    
    func pickerView(_ pickerView: UIPickerView, didSelectRow row: Int, inComponent component: Int) {
        switch row {
        case INGREDIENTS:
            scrapeType = .ingredient
            break
        case STEPS:
            scrapeType = .step
            break
        default: break
        }
    }
    
    func textViewDidEndEditing(_ textView: UITextView) {
        scrapeInfo?.value = textView.text
    }
    
    @objc func keyboardWillShow(notification: NSNotification) {
        beforeHeight = keyboardWillShowInternal(subview: text, notification: notification)
    }
    
    @objc func keyboardWillHide(notification: NSNotification) {
        keyboardWillHideInternal(heightMay: beforeHeight, notification: notification)
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        switch scrapeType {
        case .ingredient:
            picker.selectRow(INGREDIENTS, inComponent: 0, animated: true)
            break
        case .step:
            picker.selectRow(STEPS, inComponent: 0, animated: true)
            break
        default: break
        }
        text.text = scrapeInfo?.value
        text.addDoneButtonOnKeyboard()
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardDidShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }
}
